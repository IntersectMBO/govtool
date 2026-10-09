import type {
  MetadataResult,
  MetadataServiceV1,
} from '@govtool/data-providers/metadata';

import { mapLimit, type Anchor } from './enrich';

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
  private filling?: Promise<void>;
  private answeredAll: boolean;

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
   * named later is asked for on the next fill, one block on.
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
   * new or due for a retry. One fill at a time: a call while one runs waits
   * for it, and the next block's fill picks up what it did not cover.
   */
  fill(anchors: readonly Anchor[]): Promise<void> {
    this.filling ??= this.runFill(anchors).finally(() => {
      this.filling = undefined;
    });
    return this.filling;
  }

  forget(hash: string, url: string): void {
    const key = keyOf(hash, url);
    if (this.entries.get(key)?.document === undefined) {
      this.entries.delete(key);
    }
  }

  private async runFill(anchors: readonly Anchor[]): Promise<void> {
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
    const due = [...named.values()].filter(
      (entry) =>
        entry.document === undefined &&
        entry.failures <= DOCUMENT_MAX_RETRIES &&
        entry.retryAt <= now,
    );
    let unanswered = 0;
    await mapLimit(due, DOCUMENT_FETCH_CONCURRENCY, async (entry) => {
      if (!(await this.attempt(metadata, entry))) unanswered += 1;
    });
    // Entries not due already have an answer, so this covers every anchor.
    if (unanswered === 0) this.answeredAll = true;
  }

  /** One fetch. False when the metadata service gave no answer. */
  private async attempt(
    metadata: MetadataServiceV1,
    entry: Entry,
  ): Promise<boolean> {
    let result: MetadataResult;
    try {
      result = await metadata.getMetadata(entry.hash, entry.url);
    } catch {
      return false;
    }

    if (result.ok && isObject(result.body)) {
      entry.document = result.body;
      entry.failure = undefined;
      entry.failures = 0;
      return true;
    }

    entry.failures += 1;
    entry.failure = result.ok
      ? // Hash-correct, but no CIP-100 document is anything but an object.
        { code: 'SCHEMA_INVALID', message: 'The document is not a JSON object' }
      : { code: result.code, message: result.message };
    entry.retryAt =
      this.now() + (DOCUMENT_RETRY_DELAYS_MS[entry.failures - 1] ?? 0);
    return true;
  }
}
