import { randomUUID } from "crypto";
import * as json5 from "json5";
import { blake2b } from "libcardano";
import { prisma } from "../config/db";
import {
  ERROR_TTL_MS, FETCH_LIMIT_BYTES, HTTP_TIMEOUT_MS, IPFS_TIMEOUT_MS, REFRESH_WINDOW_MS,
} from "../config";
import { fetchWithReport, type FetchHop, type FetchOutcome } from "../helpers/fetchWithReport";
import { blacklistGateway, gatewayOrder, gatewayUrl, parseIpfsUrl, type IpfsAddress } from "../helpers/ipfs";
import { PositionIndex, type SourceRange } from "../helpers/positions";
import {
  FAILURE_CATEGORY, isFailureCode, type MetadataFailureCategory, type MetadataFailureCode,
} from "../helpers/Errors";

/* -- result shapes: the contract's MetadataResult ---------------------------- */

export interface ContentIssue {
  field?: string;
  reason: string;
  range?: SourceRange;
}

export interface MetadataSuccess {
  ok: true;
  hash: string;
  body: unknown;
  fetchedAt: string;
  /** Where the content was first fetched from. Informational only. */
  url?: string | null;
}

export interface MetadataFailure {
  ok: false;
  code: MetadataFailureCode;
  category: MetadataFailureCategory;
  message: string;
  servedHash?: string;
  reportId?: string;
  checkedAt: string;
}

export type MetadataResult = MetadataSuccess | MetadataFailure;

export interface RefreshOutcome {
  refetched: boolean;
  retryAfterSeconds?: number;
  result: MetadataResult;
}

const hex = (value: Uint8Array | null | undefined) =>
  value ? Buffer.from(value).toString("hex") : undefined;

/* -- analysing a fetch -------------------------------------------------------- */

interface ReceivedBody {
  bytes: Buffer;
  truncated: boolean;
  contentType?: string;
}

type Analysis =
  | { ok: true; servedHash: Buffer; raw: Buffer; document: unknown }
  | {
      ok: false;
      code: MetadataFailureCode;
      message: string;
      issues: ContentIssue[];
      servedHash?: Buffer;
      body?: ReceivedBody;
    };

const contentTypeOf = (headers: Record<string, string | string[]>) => {
  const value = headers["content-type"];
  return Array.isArray(value) ? value[0] : value;
};

const analyse = (requested: Buffer, outcome: Awaited<ReturnType<typeof fetchWithReport>>): Analysis => {
  const response = outcome.response;
  if (!response) {
    return { ok: false, code: "FETCH_ERROR", message: outcome.failure ?? "Fetch failed", issues: [] };
  }
  const body: ReceivedBody | undefined = response.body.length
    ? { bytes: response.body, truncated: response.truncated, contentType: contentTypeOf(response.headers) }
    : undefined;

  if (response.status !== 200) {
    return {
      ok: false, code: "FETCH_ERROR", message: `Unexpected Status code: ${response.status}`, issues: [], body,
    };
  }
  if (response.truncated) {
    return {
      ok: false,
      code: "EXCEEDS_LIMIT",
      message: `Server responded with data larger than ${FETCH_LIMIT_BYTES / 1024}KB`,
      issues: [],
      body,
    };
  }
  if (!body) return { ok: false, code: "FETCH_ERROR", message: "Empty response from server", issues: [] };

  const text = body.bytes.toString("utf-8");
  const index = new PositionIndex(text);
  let document: any;
  try {
    document = json5.parse(text);
  } catch (e: any) {
    return {
      ok: false,
      code: "JSON_PARSE_ERROR",
      message: "Unable to parse data into JSON",
      issues: [{
        reason: String(e?.message ?? e).replace(/^JSON5: /, ""),
        range: index.rangeAtLineColumn(e?.lineNumber, e?.columnNumber),
      }],
      body,
    };
  }

  // CIP-100: hashAlgorithm, when present, must be the string "blake2b-256",
  // possibly wrapped as a JSON-LD @value. This moves under ?cip= with the rest
  // of validation.
  const field = document !== null && typeof document === "object" ? document.hashAlgorithm : undefined;
  const hashAlg = field !== null && typeof field === "object" && "@value" in field ? field["@value"] : field;
  if (hashAlg !== undefined && hashAlg !== null) {
    const range = index.rangeOfPath(["hashAlgorithm"]);
    if (typeof hashAlg !== "string") {
      const reason = `Invalid type of field $.hashAlgorithm: ${typeof hashAlg}`;
      return { ok: false, code: "SCHEMA_INVALID", message: reason, issues: [{ field: "hashAlgorithm", reason, range }], body };
    }
    if (hashAlg !== "blake2b-256") {
      const reason = `Metadata uses unknown hashAlgorithm: ${hashAlg}`;
      return { ok: false, code: "SCHEMA_INVALID", message: reason, issues: [{ field: "hashAlgorithm", reason, range }], body };
    }
  }

  const servedHash = Buffer.from(blake2b.hash32(body.bytes));
  if (!servedHash.equals(requested)) {
    return {
      ok: false, code: "HASH_MISMATCH", message: "Hash of fetched data does not match", issues: [], servedHash, body,
    };
  }
  return { ok: true, servedHash, raw: body.bytes, document };
};

/* -- storage ------------------------------------------------------------------ */

/** Content is written once per hash. The hash is the identity of the bytes. */
const storeContent = async (hash: Buffer, raw: Buffer, url: string) => {
  const existing = await prisma.metadata.findFirst({
    where: { hash: Uint8Array.from(hash), error: null }, select: { id: true },
  });
  if (existing) return;
  await prisma.metadata.create({
    data: { hash: Uint8Array.from(hash), data: Uint8Array.from(raw), url, fetchedAt: new Date() },
  });
};

const storeReport = async (
  requested: Buffer, url: string, effectiveUrl: string, hops: FetchHop[],
  failure: Extract<Analysis, { ok: false }>, startedAt: Date, finishedAt: Date,
): Promise<string> => {
  let bodyHash: Buffer | undefined;
  if (failure.code === "HASH_MISMATCH" && failure.servedHash) {
    // The served bytes are cached as content under servedHash; not stored twice.
    bodyHash = failure.servedHash;
  } else if (failure.body) {
    bodyHash = Buffer.from(blake2b.hash32(failure.body.bytes));
    await prisma.fetch_body.createMany({
      data: [{ hash: Uint8Array.from(bodyHash), data: Uint8Array.from(failure.body.bytes) }],
      skipDuplicates: true,
    });
  }
  const id = randomUUID();
  await prisma.fetch_report.create({
    data: {
      id,
      hash: Uint8Array.from(requested),
      url,
      code: failure.code,
      message: failure.message,
      servedHash: failure.servedHash ? Uint8Array.from(failure.servedHash) : undefined,
      bodyHash: bodyHash ? Uint8Array.from(bodyHash) : undefined,
      startedAt,
      finishedAt,
      details: JSON.parse(JSON.stringify({
        effectiveUrl,
        hops,
        body: failure.body
          ? { size: failure.body.bytes.length, truncated: failure.body.truncated, contentType: failure.body.contentType }
          : undefined,
        issues: failure.issues,
      })),
    },
  });
  return id;
};

/* -- reads -------------------------------------------------------------------- */

/** Content under the hash is authoritative: the url plays no part. */
export const findContent = async (hash: Buffer): Promise<MetadataSuccess | undefined> => {
  const row = await prisma.metadata.findFirst({
    where: { hash: Uint8Array.from(hash), error: null },
    orderBy: { fetchedAt: "asc" },
  });
  if (!row?.data) return undefined;
  return {
    ok: true,
    hash: hash.toString("hex"),
    body: json5.parse(Buffer.from(row.data).toString("utf-8")),
    fetchedAt: row.fetchedAt.toISOString(),
    url: row.url,
  };
};

/** The latest failure for exactly this (url, hash) pair, with its age. */
export const findLatestFailure = async (url: string, hash: Buffer) => {
  const row = await prisma.metadata.findFirst({
    where: { url, hash: Uint8Array.from(hash), error: { not: null } },
    orderBy: { fetchedAt: "desc" },
  });
  if (!row) return undefined;
  const result: MetadataFailure = {
    ok: false,
    code: isFailureCode(row.code) ? row.code : "FETCH_ERROR",
    category: FAILURE_CATEGORY[isFailureCode(row.code) ? row.code : "FETCH_ERROR"],
    message: row.error!,
    servedHash: hex(row.servedHash),
    reportId: row.reportId ?? undefined,
    checkedAt: row.fetchedAt.toISOString(),
  };
  return { result, ageMs: Date.now() - row.fetchedAt.getTime() };
};

/* -- the fetch ---------------------------------------------------------------- */

const timeoutFor = (isIpfs: boolean) => {
  const configured = Number.parseInt(process.env.METADATA_REQUEST_TIMEOUT_MS || "", 10);
  if (Number.isFinite(configured) && configured > 0) return configured;
  return isIpfs ? IPFS_TIMEOUT_MS : HTTP_TIMEOUT_MS;
};

const hostOf = (gateway: string) => {
  try {
    return new URL(gateway).host;
  } catch {
    return gateway;
  }
};

/**
 * Fetch IPFS content through the service's gateways, in order. Gateways are
 * trusted (D126): the first `200` ends the search, whatever it contains. Any
 * other outcome moves on to the next gateway, and a `429` or `503` also
 * blacklists that gateway for a while. Every gateway tried is a hop in the
 * one report.
 */
const fetchIpfs = async (address: IpfsAddress): Promise<{ outcome: FetchOutcome; effectiveUrl: string }> => {
  const { order, skipped } = gatewayOrder();
  const skippedNote = skipped.length
    ? `; skipped as unavailable: ${skipped.map(hostOf).join(", ")}`
    : "";
  if (order.length === 0) {
    return {
      outcome: { hops: [], failure: `All IPFS gateways are temporarily unavailable${skippedNote}` },
      effectiveUrl: `${address.namespace}://${address.id}${address.rest}`,
    };
  }
  const effectiveUrl = gatewayUrl(order[0], address);
  const hops: FetchHop[] = [];
  const notes: string[] = [];
  for (const gateway of order) {
    const outcome = await fetchWithReport(gatewayUrl(gateway, address), timeoutFor(true));
    hops.push(...outcome.hops);
    const status = outcome.response?.status;
    if (status === 200) return { outcome: { hops, response: outcome.response }, effectiveUrl };
    if (status === 429 || status === 503) blacklistGateway(gateway);
    notes.push(`${hostOf(gateway)}: ${status ? `HTTP ${status}` : outcome.failure}`);
  }
  return {
    outcome: { hops, failure: `No IPFS gateway served the content (${notes.join("; ")})${skippedNote}` },
    effectiveUrl,
  };
};

/** Fetch `url` for `hash` now, and record what happened. */
export const fetchAndRecord = async (hash: Buffer, url: string): Promise<MetadataResult> => {
  const ipfs = parseIpfsUrl(url);
  const startedAt = new Date();
  const { outcome, effectiveUrl } = ipfs
    ? await fetchIpfs(ipfs)
    : { outcome: await fetchWithReport(url, timeoutFor(false)), effectiveUrl: url };
  const analysis = analyse(hash, outcome);
  const finishedAt = new Date();

  if (analysis.ok) {
    await storeContent(analysis.servedHash, analysis.raw, url);
    return { ok: true, hash: hash.toString("hex"), body: analysis.document, fetchedAt: finishedAt.toISOString(), url };
  }

  // The bytes are valid content for the hash they actually have (D112).
  if (analysis.code === "HASH_MISMATCH" && analysis.servedHash && analysis.body) {
    await storeContent(analysis.servedHash, analysis.body.bytes, url);
  }
  const reportId = await storeReport(hash, url, effectiveUrl, outcome.hops, analysis, startedAt, finishedAt);
  // The failure itself only describes the url at this moment: replayed briefly.
  await prisma.metadata.create({
    data: {
      hash: Uint8Array.from(hash),
      url,
      code: analysis.code,
      error: analysis.message,
      fetchedAt: finishedAt,
      reportId,
      servedHash: analysis.servedHash ? Uint8Array.from(analysis.servedHash) : undefined,
    },
  });
  console.error("[ERROR] fetch", { reportId, code: analysis.code, url, message: analysis.message });
  return {
    ok: false,
    code: analysis.code,
    category: FAILURE_CATEGORY[analysis.code],
    message: analysis.message,
    servedHash: analysis.servedHash?.toString("hex"),
    reportId,
    checkedAt: finishedAt.toISOString(),
  };
};

/**
 * Resolve for the legacy GET route. Cached content wins outright. Otherwise a
 * recent failure for (url, hash) is replayed for the error TTL, or with
 * `invalidate` for the refresh window, which a request cannot shorten.
 */
export const resolve = async (hash: Buffer, url: string | undefined, invalidate: boolean) => {
  const content = await findContent(hash);
  if (content) return content;
  if (!url) return undefined;
  const latest = await findLatestFailure(url, hash);
  const window = invalidate ? REFRESH_WINDOW_MS : ERROR_TTL_MS;
  if (latest && latest.ageMs < window) return latest.result;
  return fetchAndRecord(hash, url);
};

/** A retry: at most one real fetch per (url, hash) per window, whoever asks. */
export const refresh = async (hash: Buffer, url: string): Promise<RefreshOutcome> => {
  const content = await findContent(hash);
  if (content) return { refetched: false, result: content };
  const latest = await findLatestFailure(url, hash);
  if (latest && latest.ageMs < REFRESH_WINDOW_MS) {
    return {
      refetched: false,
      retryAfterSeconds: Math.max(1, Math.ceil((REFRESH_WINDOW_MS - latest.ageMs) / 1000)),
      result: latest.result,
    };
  }
  return { refetched: true, result: await fetchAndRecord(hash, url) };
};

/* -- reports ------------------------------------------------------------------ */

const encodeBody = (bytes: Buffer) => {
  try {
    return { encoding: "utf8" as const, data: new TextDecoder("utf-8", { fatal: true }).decode(bytes) };
  } catch {
    return { encoding: "base64" as const, data: bytes.toString("base64") };
  }
};

export const getReport = async (id: string) => {
  const row = await prisma.fetch_report.findUnique({ where: { id } });
  if (!row) return null;
  const details = row.details as any;
  let body: object | undefined;
  if (row.bodyHash) {
    const stored = await prisma.fetch_body.findUnique({ where: { hash: row.bodyHash } });
    const bytes = stored?.data
      ?? (await prisma.metadata.findFirst({ where: { hash: row.bodyHash, error: null } }))?.data;
    if (bytes) {
      const buf = Buffer.from(bytes);
      body = {
        hash: hex(row.bodyHash),
        size: buf.length,
        truncated: details.body?.truncated ?? false,
        contentType: details.body?.contentType,
        ...encodeBody(buf),
      };
    }
  }
  const code: MetadataFailureCode = isFailureCode(row.code) ? row.code : "FETCH_ERROR";
  return {
    id: row.id,
    hash: hex(row.hash)!,
    url: row.url,
    effectiveUrl: details.effectiveUrl ?? row.url,
    startedAt: row.startedAt.toISOString(),
    finishedAt: row.finishedAt.toISOString(),
    hops: details.hops ?? [],
    body,
    result: {
      code,
      category: FAILURE_CATEGORY[code],
      message: row.message,
      servedHash: hex(row.servedHash),
      issues: details.issues ?? [],
    },
  };
};

export const listReports = async (hash: Buffer, url: string) => {
  const rows = await prisma.fetch_report.findMany({
    where: { url, hash: Uint8Array.from(hash) },
    orderBy: { startedAt: "desc" },
    select: { id: true, startedAt: true, code: true, message: true },
  });
  return rows.map((r) => {
    const code: MetadataFailureCode = isFailureCode(r.code) ? r.code : "FETCH_ERROR";
    return { id: r.id, startedAt: r.startedAt.toISOString(), code, category: FAILURE_CATEGORY[code], message: r.message };
  });
};
