import * as dns from "dns";
import * as http from "http";
import * as https from "https";
import * as net from "net";
import { classifyAddress } from "./addressGuard";
import { FETCH_LIMIT_BYTES, REDIRECT_LIMIT } from "../config";

export type ConnectOutcome =
  | "connected" | "blocked" | "refused" | "reset" | "unreachable" | "timeout" | "tls_error" | "error";

export type TimeoutStage = "connect" | "tls" | "first_byte" | "body";

export interface ConnectAttempt {
  address: string;
  family: 4 | 6;
  outcome: ConnectOutcome;
  errorCode?: string;
  message?: string;
  blockedRange?: string;
  timeoutStage?: TimeoutStage;
  timings: { connectMs?: number; tlsMs?: number; firstByteMs?: number; endMs?: number };
}

export interface FetchHop {
  url: string;
  dns:
    | { addresses: { address: string; family: 4 | 6 }[] }
    | { error: { code: string; message: string } };
  attempts: ConnectAttempt[];
  response?: { status: number; headers: Record<string, string | string[]> };
  redirectTo?: string;
}

export interface FetchResponse {
  status: number;
  headers: Record<string, string | string[]>;
  body: Buffer;
  /** True when reading stopped at the limit. */
  truncated: boolean;
}

export interface FetchOutcome {
  hops: FetchHop[];
  /** The final hop's response, when one arrived. */
  response?: FetchResponse;
  /** Why no final response exists: one line, for the failure message. */
  failure?: string;
}

const REDIRECTS = new Set([301, 302, 303, 307, 308]);

const TLS_CODES = new Set([
  "DEPTH_ZERO_SELF_SIGNED_CERT", "SELF_SIGNED_CERT_IN_CHAIN", "UNABLE_TO_VERIFY_LEAF_SIGNATURE",
  "UNABLE_TO_GET_ISSUER_CERT", "UNABLE_TO_GET_ISSUER_CERT_LOCALLY", "HOSTNAME_MISMATCH",
]);

const isTlsCode = (code?: string) =>
  !!code && (TLS_CODES.has(code) || code.startsWith("CERT_") || code.startsWith("ERR_TLS_")
    || code.startsWith("ERR_SSL_"));

const outcomeFor = (code: string | undefined, stage: TimeoutStage): ConnectOutcome => {
  switch (code) {
    case "ECONNREFUSED": return "refused";
    case "ECONNRESET": case "EPIPE": return "reset";
    case "EHOSTUNREACH": case "ENETUNREACH": case "EHOSTDOWN": case "ENETDOWN": case "EADDRNOTAVAIL":
      return "unreachable";
    case "ETIMEDOUT": return "timeout";
  }
  if (isTlsCode(code) || stage === "tls") return "tls_error";
  return "error";
};

const headersOf = (res: http.IncomingMessage) => {
  const out: Record<string, string | string[]> = {};
  for (const [k, v] of Object.entries(res.headers)) if (v !== undefined) out[k] = v;
  return out;
};

const resolve = (host: string): Promise<FetchHop["dns"]> => {
  const literal = net.isIP(host.replace(/^\[|\]$/g, ""));
  if (literal) {
    return Promise.resolve({ addresses: [{ address: host.replace(/^\[|\]$/g, ""), family: literal as 4 | 6 }] });
  }
  return new Promise((done) => {
    dns.lookup(host, { all: true, verbatim: true }, (err, addresses) => {
      if (err) {
        done({ error: { code: (err as NodeJS.ErrnoException).code ?? "EDNS", message: err.message } });
      } else {
        done({ addresses: addresses.map((a) => ({ address: a.address, family: a.family as 4 | 6 })) });
      }
    });
  });
};

type AttemptResult =
  | { attempt: ConnectAttempt; response: FetchResponse }
  | { attempt: ConnectAttempt; response?: undefined };

/** One request to one checked address. The socket is pinned to that address. */
const attemptOne = (
  url: URL, address: string, family: 4 | 6, timeoutMs: number, readBody: (status: number) => boolean,
): Promise<AttemptResult> => new Promise((done) => {
  const started = Date.now();
  const attempt: ConnectAttempt = { address, family, outcome: "connected", timings: {} };
  const isHttps = url.protocol === "https:";
  let stage: TimeoutStage = "connect";
  let settled = false;
  const finish = (result: AttemptResult) => {
    if (settled) return;
    settled = true;
    attempt.timings.endMs = Date.now() - started;
    done(result);
  };
  const fail = (outcome: ConnectOutcome, err?: NodeJS.ErrnoException, message?: string) => {
    // Destroying a settled request raises its own error; it must not rewrite
    // what already happened.
    if (settled) return;
    attempt.outcome = outcome;
    if (err?.code) attempt.errorCode = err.code;
    attempt.message = message ?? err?.message;
    finish({ attempt });
  };

  const hostname = url.hostname.replace(/^\[|\]$/g, "");
  const options: https.RequestOptions = {
    method: "GET",
    host: hostname,
    port: url.port || (isHttps ? 443 : 80),
    path: `${url.pathname}${url.search}`,
    headers: { "user-agent": "drep-metadata-api", accept: "application/json, */*" },
    agent: false,
    // Connect to the address that was checked, never a fresh DNS answer.
    lookup: ((_host: string, opts: dns.LookupOptions, cb: any) => {
      if (opts && opts.all) cb(null, [{ address, family }]);
      else cb(null, address, family);
    }) as any,
  };
  if (isHttps && !net.isIP(hostname)) options.servername = hostname;

  const req = (isHttps ? https : http).request(options, (res) => {
    attempt.timings.firstByteMs = Date.now() - started;
    stage = "body";
    const status = res.statusCode ?? 0;
    const headers = headersOf(res);
    if (!readBody(status)) {
      res.resume();
      req.destroy();
      finish({ attempt, response: { status, headers, body: Buffer.alloc(0), truncated: false } });
      return;
    }
    const chunks: Buffer[] = [];
    let size = 0;
    let truncated = false;
    res.on("data", (chunk: Buffer) => {
      if (truncated) return;
      const room = FETCH_LIMIT_BYTES - size;
      if (chunk.length > room) {
        chunks.push(chunk.subarray(0, room));
        size += room;
        truncated = true;
        finish({ attempt, response: { status, headers, body: Buffer.concat(chunks, size), truncated } });
        req.destroy();
        return;
      }
      chunks.push(chunk);
      size += chunk.length;
    });
    res.on("end", () => finish({ attempt, response: { status, headers, body: Buffer.concat(chunks, size), truncated } }));
    res.on("error", (err: NodeJS.ErrnoException) => fail(outcomeFor(err.code, stage), err));
  });

  req.on("socket", (socket) => {
    socket.on("connect", () => {
      attempt.timings.connectMs = Date.now() - started;
      stage = isHttps ? "tls" : "first_byte";
    });
    socket.on("secureConnect", () => {
      attempt.timings.tlsMs = Date.now() - started;
      stage = "first_byte";
    });
  });
  req.setTimeout(timeoutMs, () => {
    if (settled) return;
    attempt.timeoutStage = stage;
    attempt.outcome = "timeout";
    attempt.message = `Timeout(${timeoutMs / 1000}sec) during ${stage.replace("_", " ")}`;
    finish({ attempt });
    req.destroy();
  });
  req.on("error", (err: NodeJS.ErrnoException) => {
    if (settled) return;
    if (err.code === "ETIMEDOUT") attempt.timeoutStage = stage;
    fail(outcomeFor(err.code, stage), err);
  });
  req.end();
});

const describe = (attempts: ConnectAttempt[], host: string): string => {
  if (attempts.every((a) => a.outcome === "blocked")) {
    const list = attempts.map((a) => `${a.address} (${a.blockedRange})`).join(", ");
    return `Refused: ${host} resolves only to non-public addresses: ${list}`;
  }
  const last = [...attempts].reverse().find((a) => a.outcome !== "blocked")!;
  const detail = last.errorCode ? `${last.errorCode}` : last.outcome;
  const tried = attempts.length > 1 ? ` (${attempts.length} addresses tried)` : "";
  return `${last.message ?? detail} [${detail} ${last.address}]${tried}`;
};

/**
 * Fetch `startUrl`, following redirects, recording every step. Never throws:
 * every failure is described in the outcome.
 */
export const fetchWithReport = async (startUrl: string, timeoutMs: number): Promise<FetchOutcome> => {
  const hops: FetchHop[] = [];
  let current = startUrl;
  for (let redirects = 0; ; redirects++) {
    let url: URL;
    try {
      url = new URL(current);
    } catch {
      return { hops, failure: `Invalid url: ${current}` };
    }
    if (url.protocol !== "http:" && url.protocol !== "https:") {
      hops.push({ url: current, dns: { addresses: [] }, attempts: [] });
      return { hops, failure: `Refused: unsupported protocol ${url.protocol}` };
    }
    const hostname = url.hostname.replace(/^\[|\]$/g, "");
    const hop: FetchHop = { url: current, dns: await resolve(url.hostname), attempts: [] };
    hops.push(hop);
    if ("error" in hop.dns) {
      return { hops, failure: `DNS lookup failed for ${hostname}: ${hop.dns.error.code}` };
    }
    if (hop.dns.addresses.length === 0) {
      return { hops, failure: `DNS lookup returned no addresses for ${hostname}` };
    }

    let response: FetchResponse | undefined;
    for (const { address, family } of hop.dns.addresses) {
      const verdict = classifyAddress(address);
      if (!verdict.public) {
        hop.attempts.push({ address, family, outcome: "blocked", blockedRange: verdict.range, timings: {} });
        continue;
      }
      const result = await attemptOne(url, address, family, timeoutMs,
        (status) => !(REDIRECTS.has(status)));
      hop.attempts.push(result.attempt);
      if (result.response) {
        response = result.response;
        break;
      }
    }
    if (!response) return { hops, failure: describe(hop.attempts, hostname) };

    hop.response = { status: response.status, headers: response.headers };
    if (REDIRECTS.has(response.status)) {
      const location = response.headers["location"];
      if (!location || Array.isArray(location)) {
        return { hops, failure: `Redirect ${response.status} without a location header` };
      }
      if (redirects >= REDIRECT_LIMIT) {
        return { hops, failure: `Maximum of ${REDIRECT_LIMIT} redirects reached` };
      }
      hop.redirectTo = new URL(location, current).href;
      current = hop.redirectTo;
      continue;
    }
    return { hops, response };
  }
};
