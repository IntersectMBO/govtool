import { Injectable, Logger } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';

type CacheEntry<T> = {
  expiresAt: number;
  value: Promise<T>;
  refreshing: boolean;
};

type NamespaceCache = Map<string, CacheEntry<unknown>>;

/**
 * The namespaces that hold one wallet's own chain state: what it registered,
 * how it delegated, what it voted. Its own transaction changes them, and the
 * frontend reads them again as soon as `/transaction/status` says it landed,
 * so an entry read before that block must not answer the read after it.
 * The shared snapshots are not here: the warmer refreshes those itself.
 */
const WALLET_STATE_NAMESPACES = [
  'accountInfo',
  'adaHolderCurrentDelegation',
  'adaHolderVotingPower',
  'drepInfo',
  'drepVoteRows',
  'drepVotes',
  'drepVotingPower',
  'proposalList',
] as const;

/** A block, with what tells it apart from another block at its height. */
export type BlockMark = { block: number; slot?: number; hash?: string };

/**
 * Whether two marks name the same block. Height alone misses a fork switch
 * at the tip, where the new block has the old one's height and, in a slot
 * battle, its slot too, so the hash decides when both carry one, else the
 * slot. A mark that carries neither is taken at its height.
 */
export function isSameBlock(a: BlockMark, b: BlockMark): boolean {
  if (a.block !== b.block) return false;
  if (a.hash !== undefined && b.hash !== undefined) return a.hash === b.hash;
  if (a.slot !== undefined && b.slot !== undefined) return a.slot === b.slot;
  return true;
}

@Injectable()
export class CacheService {
  private readonly logger = new Logger(CacheService.name);

  /*
   * Each namespace has its own LRU partition. Activity in one namespace
   * cannot evict entries from another namespace.
   */
  private readonly caches = new Map<string, NamespaceCache>();

  /** The block the wallet state was last cleared for. */
  private mark: BlockMark | null = null;

  constructor(private readonly configService: ConfigService) {}

  /**
   * Drops the wallet-state namespaces when the warmer reads a tip it has not
   * seen. A tip below the last one is a rollback, or a chain reset under a
   * running backend (a devnet restart, a db-sync restore), and one at the
   * same height with another hash is a fork switch: each clears too, and
   * becomes the mark, so later blocks clear again instead of waiting for the
   * chain to pass its old height. The same block read again only fills in
   * what the mark lacked, such as the hash a transaction's block came without.
   */
  noteTip(tip: BlockMark): void {
    const seen = this.mark !== null && isSameBlock(tip, this.mark);

    this.mark = tip;

    if (!seen) {
      this.clearWalletState();
    }
  }

  /**
   * Drops the wallet-state namespaces for the block a confirmed transaction
   * landed in, which can be ahead of the warmer's last tick. Anyone can ask
   * for a transaction's status, so only a higher block clears: at most once
   * per block, and an old transaction cannot move the mark back.
   */
  noteBlock(block: BlockMark): void {
    if (this.mark !== null && block.block <= this.mark.block) {
      return;
    }

    this.mark = block;
    this.clearWalletState();
  }

  getOrSet<T>(
    namespace: string,
    key: unknown,
    action: () => Promise<T>,
    ttlSeconds = this.defaultTtlSeconds(),
  ): Promise<T> {
    const cache = this.getNamespaceCache(namespace);
    const cacheKey = this.toCacheKey(key);
    const now = Date.now();

    const entry = cache.get(cacheKey) as CacheEntry<T> | undefined;

    if (entry && entry.expiresAt > now) {
      this.touch(cache, cacheKey, entry);
      return entry.value;
    }

    const value = action().catch((error: unknown) => {
      if (cache.get(cacheKey)?.value === value) {
        cache.delete(cacheKey);
      }

      throw error;
    });

    this.store(namespace, cacheKey, {
      expiresAt: now + ttlSeconds * 1_000,
      value,
      refreshing: false,
    });

    return value;
  }

  async getOrSetStaleWhileRevalidate<T>(
    namespace: string,
    key: unknown,
    action: () => Promise<T>,
    ttlSeconds = this.defaultTtlSeconds(),
  ): Promise<T> {
    const cache = this.getNamespaceCache(namespace);
    const cacheKey = this.toCacheKey(key);
    const entry = cache.get(cacheKey) as CacheEntry<T> | undefined;
    const now = Date.now();

    if (!entry) {
      return this.getOrSet(namespace, key, action, ttlSeconds);
    }

    this.touch(cache, cacheKey, entry);

    if (entry.expiresAt > now) {
      return entry.value;
    }

    if (!entry.refreshing) {
      entry.refreshing = true;

      void action()
        .then((value) => {
          if (cache.get(cacheKey) === entry) {
            this.set(namespace, key, value, ttlSeconds);
          }
        })
        .catch((error: unknown) => {
          this.logger.error(
            `Failed to refresh cache ${namespace}:${cacheKey}`,
            error instanceof Error ? error.stack : String(error),
          );
        })
        .finally(() => {
          const latest = cache.get(cacheKey);

          if (latest === entry) {
            latest.refreshing = false;
          }
        });
    }

    return entry.value;
  }

  async refresh<T>(
    namespace: string,
    key: unknown,
    action: () => Promise<T>,
    ttlSeconds = this.defaultTtlSeconds(),
  ): Promise<T> {
    const value = await action();

    this.set(namespace, key, value, ttlSeconds);

    return value;
  }

  set<T>(
    namespace: string,
    key: unknown,
    value: T,
    ttlSeconds = this.defaultTtlSeconds(),
  ): void {
    const cacheKey = this.toCacheKey(key);

    this.store(namespace, cacheKey, {
      expiresAt: Date.now() + ttlSeconds * 1_000,
      value: Promise.resolve(value),
      refreshing: false,
    });
  }

  delete(namespace: string, key: unknown): void {
    const cache = this.caches.get(namespace);

    if (!cache) {
      return;
    }

    cache.delete(this.toCacheKey(key));

    if (cache.size === 0) {
      this.caches.delete(namespace);
    }
  }

  clear(namespace?: string): void {
    if (namespace !== undefined) {
      this.caches.delete(namespace);
      return;
    }

    this.caches.clear();
  }

  defaultTtlSeconds(): number {
    return this.configService.get().cacheDurationSeconds;
  }

  drepListTtlSeconds(): number {
    return this.configService.get().drepListCacheDurationSeconds;
  }

  private clearWalletState(): void {
    for (const namespace of WALLET_STATE_NAMESPACES) {
      this.caches.delete(namespace);
    }
  }

  private getNamespaceCache(namespace: string): NamespaceCache {
    let cache = this.caches.get(namespace);

    if (!cache) {
      cache = new Map<string, CacheEntry<unknown>>();
      this.caches.set(namespace, cache);
    }

    return cache;
  }

  private touch(
    cache: NamespaceCache,
    key: string,
    entry: CacheEntry<unknown>,
  ): void {
    cache.delete(key);
    cache.set(key, entry);
  }

  /**
   * Bounded, because every distinct key is a cache entry that nothing else
   * removes: DRep ids, stake keys and proposal ids all come from the request
   * path, so an unbounded Map grows with the number of distinct identifiers
   * an internet client cares to ask for. The bound is per namespace.
   */
  private store(
    namespace: string,
    key: string,
    entry: CacheEntry<unknown>,
  ): void {
    const cache = this.getNamespaceCache(namespace);

    this.touch(cache, key, entry);

    const maxEntries = this.configService.get().cacheMaxEntries;

    while (cache.size > maxEntries) {
      const oldestKey = cache.keys().next();

      if (oldestKey.done) {
        break;
      }

      cache.delete(oldestKey.value);
    }
  }

  private toCacheKey(key: unknown): string {
    return JSON.stringify(key) ?? 'undefined';
  }
}
