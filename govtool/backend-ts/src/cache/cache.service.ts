import { Injectable, Logger } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';

type CacheEntry<T> = {
  expiresAt: number;
  value: Promise<T>;
  refreshing: boolean;
};

type NamespaceCache = Map<string, CacheEntry<unknown>>;

@Injectable()
export class CacheService {
  private readonly logger = new Logger(CacheService.name);

  /*
   * Each namespace has its own LRU partition. Activity in one namespace
   * cannot evict entries from another namespace.
   */
  private readonly caches = new Map<string, NamespaceCache>();

  constructor(private readonly configService: ConfigService) {}

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
