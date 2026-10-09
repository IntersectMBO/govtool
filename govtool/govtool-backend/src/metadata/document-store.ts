import type {
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import type { Anchor } from './enrich';

/**
 * Every anchored document a snapshot names, fetched ahead of the requests
 * that show it. The DRep directory and the governance action list, their
 * searches and a DRep's vote history read documents from here and never
 * fetch while a request waits.
 *
 * A resolved document is kept for good: its content is fixed by its hash.
 * A failure the metadata service reports (an unreachable url, a hash
 * mismatch, ...) is tried again later, up to DOCUMENT_MAX_RETRIES times, and
 * then kept as the anchor's answer with its reason, until the anchor changes
 * or a retry through the metadata routes resolves it (`forgetDocumentFailure`).
 * When the metadata service itself gives no answer, nothing is learned about
 * the document, so the attempt is not counted and the next fill asks again.
 *
 * Only `fill` adds entries, and it drops those no snapshot names any more, so
 * the store holds the documents of the current snapshots and nothing else.
 */

export type Json = Record<string, unknown>;

export type DocumentFailure = { code: string; message: string };

export type StoredDocument =
  | { status: 'resolved'; document: Json }
  /** Named by a snapshot, with no answer from the metadata service yet. */
  | { status: 'pending' }
  /** `final` once the retries are spent. */
  | { status: 'failed'; failure: DocumentFailure; final: boolean };

/** Documents asked of the metadata service at once. */
export const DOCUMENT_FETCH_CONCURRENCY = 30;
/**
 * How long a fill waits for the metadata service's answer on one document.
 * Nobody waits on a fill, so it outlasts the service's own fetch: a host that
 * stalls one stage (40 s idle limit for http) or every IPFS gateway in turn
 * (four at 15 s). Shorter, a slow but valid anchor would be given up on before
 * the service answered, and its DRep would wait a block for its name.
 */
export const DOCUMENT_FETCH_TIMEOUT_MS = 120_000;
/** Retries after a first failure, before the failure is final. */
export const DOCUMENT_MAX_RETRIES = 3;
/** The wait before each retry. */
export const DOCUMENT_RETRY_DELAYS_MS = [
  5 * 60_000,
  10 * 60_000,
  20 * 60_000,
] as const;

type Entry = {
  hash: string;
  url: string;
  document?: Json;
  failure?: DocumentFailure;
  /** Failures the metadata service reported, in a row. */
  failures: number;
  /** When the next attempt may start. */
  retryAt: number;
  /** The attempt under way, which a fill named it again joins. */
  inFlight?: Promise<void>;
};

const isObject = (value: unknown): value is Json =>
  value !== null && typeof value === 'object' && !Array.isArray(value);

const keyOf = (hash: string, url: string) => `${hash.toLowerCase()} ${url}`;

/** Every store, so a retry through the metadata routes reaches each one. */
const stores = new Set<DocumentStore>();

/**
 * Called when a retry through the metadata routes resolves an anchor, such as
 * after its publisher fixed the url: every store holding a failure for it
 * asks again on its next fill.
 */
export function forgetDocumentFailure(hash: string, url: string): void {
  for (const store of stores) {
    store.forget(hash, url);
  }
}

export class DocumentStore {
  private readonly entries = new Map<string, Entry>();
  private answeredAll: boolean;
  /** Fetches under way, across fills. */
  private active = 0;
  /** Fetches waiting for one of the DOCUMENT_FETCH_CONCURRENCY slots. */
  private readonly waiting: (() => void)[] = [];

  constructor(
    private readonly metadata: MetadataServiceV1 | null,
    private readonly now: () => number = Date.now,
  ) {
    // Without a metadata service there is no document to wait for.
    this.answeredAll = metadata === null;
    stores.add(this);
  }

  /**
   * Whether every anchor named so far has had an answer from the metadata
   * service. Until then a search would leave out documents nobody has asked
   * for yet, so it refuses instead. Once true it stays true: an anchor first
   * named later is asked for by the fill that names it.
   */
  get ready(): boolean {
    return this.answeredAll;
  }

  get(anchor: Anchor): StoredDocument | undefined {
    if (!anchor.url || !anchor.hash) return undefined;
    const entry = this.entries.get(keyOf(anchor.hash, anchor.url));
    if (entry === undefined) return undefined;
    if (entry.document !== undefined) {
      return { status: 'resolved', document: entry.document };
    }
    if (entry.failure !== undefined) {
      return {
        status: 'failed',
        failure: entry.failure,
        final: entry.failures > DOCUMENT_MAX_RETRIES,
      };
    }
    return { status: 'pending' };
  }

  /** The resolved document, or undefined for any other state. */
  document(anchor: Anchor): Json | undefined {
    const stored = this.get(anchor);
    return stored?.status === 'resolved' ? stored.document : undefined;
  }

  /**
   * Makes `anchors` the store's whole set and asks for every document that is
   * new or due for a retry, resolving once those have settled. Each anchor is
   * fetched on its own under one limit shared by every fill, so a document
   * that takes long holds a slot, not the next block's new anchors.
   */
  async fill(anchors: readonly Anchor[]): Promise<void> {
    const metadata = this.metadata;
    if (metadata === null) return;

    const named = new Map<string, Entry>();
    for (const { url, hash } of anchors) {
      if (!url || !hash) continue;
      const key = keyOf(hash, url);
      if (named.has(key)) continue;
      named.set(
        key,
        this.entries.get(key) ?? {
          hash: hash.toLowerCase(),
          url,
          failures: 0,
          retryAt: 0,
        },
      );
    }
    // An anchor no snapshot names any more (a DRep that changed its anchor)
    // goes with this fill.
    this.entries.clear();
    for (const [key, entry] of named) {
      this.entries.set(key, entry);
    }

    const now = this.now();
    const settling = [...named.values()].flatMap((entry) => {
      if (entry.inFlight !== undefined) return [entry.inFlight];
      if (
        entry.document !== undefined ||
        entry.failures > DOCUMENT_MAX_RETRIES ||
        entry.retryAt > now
      ) {
        return [];
      }
      const attempt = this.limited(() => this.attempt(metadata, entry));
      entry.inFlight = attempt.finally(() => {
        entry.inFlight = undefined;
      });
      return [entry.inFlight];
    });
    await Promise.all(settling);

    if (
      [...this.entries.values()].every(
        (entry) => entry.document !== undefined || entry.failure !== undefined,
      )
    ) {
      this.answeredAll = true;
    }
  }

  forget(hash: string, url: string): void {
    const key = keyOf(hash, url);
    if (this.entries.get(key)?.document === undefined) {
      this.entries.delete(key);
    }
  }

  /** Runs `task` in one of the DOCUMENT_FETCH_CONCURRENCY slots. */
  private async limited(task: () => Promise<void>): Promise<void> {
    if (this.active < DOCUMENT_FETCH_CONCURRENCY) {
      this.active += 1;
    } else {
      // Handed the slot of the fetch that finishes next.
      await new Promise<void>((resolve) => this.waiting.push(resolve));
    }
    try {
      await task();
    } finally {
      const next = this.waiting.shift();
      if (next !== undefined) next();
      else this.active -= 1;
    }
  }

  /**
   * One fetch. When the metadata service gives no answer the entry is left as
   * it was, so the next fill asks again.
   */
  private async attempt(
    metadata: MetadataServiceV1,
    entry: Entry,
  ): Promise<void> {
    // Dropped by a later fill while it waited for a slot.
    if (this.entries.get(keyOf(entry.hash, entry.url)) !== entry) return;

    let result: MetadataResult;
    try {
      result = await metadata.getMetadata(entry.hash, entry.url);
    } catch {
      return;
    }

    if (result.ok && isObject(result.body)) {
      entry.document = result.body;
      entry.failure = undefined;
      entry.failures = 0;
      return;
    }

    entry.failures += 1;
    entry.failure = result.ok
      ? // Hash-correct, but no CIP-100 document is anything but an object.
        { code: 'SCHEMA_INVALID', message: 'The document is not a JSON object' }
      : { code: result.code, message: result.message };
    entry.retryAt =
      this.now() + (DOCUMENT_RETRY_DELAYS_MS[entry.failures - 1] ?? 0);
  }
}
