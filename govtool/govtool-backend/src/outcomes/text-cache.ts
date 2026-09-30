/**
 * The title and abstract of anchored documents, kept by (hash, url) so an
 * outcomes search does not ask the metadata service for every action's
 * document on each request.
 *
 * Stale-while-revalidate: only the first fetch of a key is awaited. Once a
 * key holds a value (a summary, or "unresolved"), an expired entry is still
 * served at once and refreshed in the background, one refresh per key and at
 * most `refreshConcurrency` at a time. The TTLs only decide when to
 * revalidate: a resolved document is keyed by its content hash, so it is
 * revalidated rarely; an unresolved one (not fetched yet, unreachable, hash
 * mismatch, over the time budget) soon, so a document the metadata service
 * fetches later is picked up. A refresh that fails or comes back unresolved
 * keeps a summary already held. Bounded, least recently used out first;
 * values are two short strings, not the documents.
 */

export type DocumentSummary = { title: string | null; abstract: string | null };

type Resolve = () => Promise<DocumentSummary | undefined>;

type Entry = {
  expiresAt: number;
  value: Promise<DocumentSummary | undefined>;
  /** Set once the first fetch has settled. */
  settled?: { summary: DocumentSummary | undefined };
  refresh?: Promise<void>;
};

export const RESOLVED_TTL_MS = 24 * 60 * 60 * 1000;
export const UNRESOLVED_TTL_MS = 5 * 60 * 1000;
export const TEXT_CACHE_MAX_ENTRIES = 4096;
export const REFRESH_CONCURRENCY = 8;

const ttl = (summary: DocumentSummary | undefined) =>
  summary === undefined ? UNRESOLVED_TTL_MS : RESOLVED_TTL_MS;

export class DocumentSummaryCache {
  private readonly entries = new Map<string, Entry>();
  private running = 0;
  private readonly queue: Array<() => void> = [];

  constructor(
    private readonly maxEntries = TEXT_CACHE_MAX_ENTRIES,
    private readonly now: () => number = Date.now,
    private readonly refreshConcurrency = REFRESH_CONCURRENCY,
  ) {}

  get size(): number {
    return this.entries.size;
  }

  /**
   * The cached summary, or `resolve`'s on a key's first fetch, shared by
   * concurrent callers. An expired value is returned as is while it is
   * refreshed in the background; `awaitRefresh` (the warmer) waits for that
   * refresh instead. A rejection is treated as unresolved.
   */
  get(
    hash: string,
    url: string,
    resolve: Resolve,
    options: { awaitRefresh?: boolean } = {},
  ): Promise<DocumentSummary | undefined> {
    const key = `${hash.toLowerCase()} ${url}`;
    const hit = this.entries.get(key);
    if (hit !== undefined) {
      this.entries.delete(key);
      this.entries.set(key, hit);
      if (hit.expiresAt > this.now()) return hit.value;
      // Expired entries have settled: in-flight first fetches never expire.
      const refresh = (hit.refresh ??= this.revalidate(key, hit, resolve));
      return options.awaitRefresh === true
        ? refresh.then(() => hit.value)
        : hit.value;
    }
    const entry: Entry = {
      // Held while in flight, so a concurrent search waits on the same fetch.
      expiresAt: Number.POSITIVE_INFINITY,
      value: resolve().catch(() => undefined),
    };
    void entry.value.then((summary) => {
      entry.settled = { summary };
      entry.expiresAt = this.now() + ttl(summary);
    });
    this.entries.set(key, entry);
    while (this.entries.size > this.maxEntries) {
      const oldest = this.entries.keys().next();
      if (oldest.done === true) break;
      this.entries.delete(oldest.value);
    }
    return entry.value;
  }

  private async revalidate(
    key: string,
    entry: Entry,
    resolve: Resolve,
  ): Promise<void> {
    try {
      const summary = await this.limited(resolve).catch(() => undefined);
      // An entry evicted or replaced meanwhile is not written back.
      if (this.entries.get(key) !== entry) return;
      const previous = entry.settled?.summary;
      if (summary !== undefined || previous === undefined) {
        entry.settled = { summary };
        entry.value = Promise.resolve(summary);
      }
      entry.expiresAt = this.now() + ttl(summary);
    } finally {
      entry.refresh = undefined;
    }
  }

  /** At most `refreshConcurrency` refreshes run; a finished one hands its slot on. */
  private async limited<T>(run: () => Promise<T>): Promise<T> {
    if (this.running < this.refreshConcurrency) this.running += 1;
    else await new Promise<void>((start) => this.queue.push(start));
    try {
      return await run();
    } finally {
      const next = this.queue.shift();
      if (next === undefined) this.running -= 1;
      else next();
    }
  }
}
