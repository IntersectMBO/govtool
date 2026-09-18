import { Injectable, Logger } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';

type CacheEntry<T> = {
  expiresAt: number;
  value: Promise<T>;
  refreshing: boolean;
};

@Injectable()
export class CacheService {
  private readonly logger = new Logger(CacheService.name);
  private readonly cache = new Map<string, CacheEntry<unknown>>();

  constructor(private readonly configSerivce: ConfigService) {}

  getOrSet<T>(
    namespace: string,
    key: unknown,
    action: () => Promise<T>,
    ttlSeconds = this.defaultTtlSeconds(),
  ): Promise<T> {
    const cacheKey = this.toCacheKey(namespace, key);
    const now = Date.now();
    const entry = this.cache.get(cacheKey) as CacheEntry<T> | undefined;

    if (entry && entry.expiresAt > now) {
      return entry.value;
    }

    const value = action().catch((error) => {
      this.cache.delete(cacheKey);
      throw error;
    });

    this.store(cacheKey, {
      expiresAt: now + ttlSeconds * 1000,
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
    const cacheKey = this.toCacheKey(namespace, key);
    const entry = this.cache.get(cacheKey) as CacheEntry<T> | undefined;
    const now = Date.now();

    if (!entry) {
      return this.refresh(namespace, key, action, ttlSeconds);
    }
    if (entry.expiresAt > now) {
      return entry.value;
    }
    if (!entry.refreshing) {
      entry.refreshing = true;
      void action()
        .then((value) => {
          this.set(namespace, key, value, ttlSeconds);
        })
        .catch((error) => {
          this.logger.error(
            `Failed to refresh cache ${cacheKey}`,
            error instanceof Error ? error.stack : String(error),
          );
        })
        .finally(() => {
          const latest = this.cache.get(cacheKey);
          if (latest) {
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
    const cacheKey = this.toCacheKey(namespace, key);

    this.store(cacheKey, {
      expiresAt: Date.now() + ttlSeconds * 1000,
      value: Promise.resolve(value),
      refreshing: false,
    });
  }

  delete(namespace: string, key: unknown): void {
    this.cache.delete(this.toCacheKey(namespace, key));
  }

  clear(): void {
    this.cache.clear();
  }

  defaultTtlSeconds(): number {
    return this.configSerivce.get().cacheDurationSeconds;
  }

  drepListTtlSeconds(): number {
    return this.configSerivce.get().drepListCacheDurationSeconds;
  }

  /**
   * Re-inserting moves the key to the end of the Map's iteration order, which
   * is what makes the eviction below least-recently-stored rather than
   * arbitrary.
   */
  private touch(key: string, entry: CacheEntry<unknown>): void {
    this.cache.delete(key);
    this.cache.set(key, entry);
  }

  /**
   * Bounded, because every distinct key is a cache entry that nothing else
   * removes: DRep ids, stake keys and proposal ids all come from the request
   * path, so an unbounded Map grows with the number of distinct identifiers
   * an internet client cares to ask for.
   */
  private store(key: string, entry: CacheEntry<unknown>): void {
    this.touch(key, entry);
    const maxEntries = this.configSerivce.get().cacheMaxEntries;
    while (this.cache.size > maxEntries) {
      const oldest = this.cache.keys().next();
      if (oldest.done === true) {
        break;
      }
      this.cache.delete(oldest.value);
    }
  }

  private toCacheKey(namespace: string, key: unknown): string {
    return `${namespace}:${JSON.stringify(key)}`;
  }
}
