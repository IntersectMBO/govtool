import { CacheService } from '../src/cache/cache.service';
import type { ConfigService } from '../src/config/config.service';

/** Only the three settings CacheService reads. */
function configWith(maxEntries: number): ConfigService {
  return {
    get: () => ({
      cacheMaxEntries: maxEntries,
      cacheDurationSeconds: 60,
      drepListCacheDurationSeconds: 60,
    }),
  } as unknown as ConfigService;
}

/** Reaches the private Map, which is the only way to observe eviction. */
function size(cache: CacheService): number {
  return (cache as unknown as { cache: Map<string, unknown> }).cache.size;
}

describe('CacheService eviction', () => {
  it('never holds more than cacheMaxEntries, however many keys arrive', () => {
    const cache = new CacheService(configWith(3));

    for (let i = 0; i < 50; i += 1) {
      cache.set('drepInfo', `drep-${i}`, i);
    }

    expect(size(cache)).toBe(3);
  });

  it('evicts the least recently stored key first', async () => {
    const cache = new CacheService(configWith(2));

    cache.set('drepInfo', 'oldest', 1);
    cache.set('drepInfo', 'middle', 2);
    // Re-storing 'oldest' makes 'middle' the least recent, so adding a third
    // key must drop 'middle' rather than 'oldest'.
    cache.set('drepInfo', 'oldest', 3);
    cache.set('drepInfo', 'newest', 4);

    expect(size(cache)).toBe(2);
    await expect(
      cache.getOrSet('drepInfo', 'oldest', () => Promise.resolve(-1)),
    ).resolves.toBe(3);
    await expect(
      cache.getOrSet('drepInfo', 'middle', () => Promise.resolve(-1)),
    ).resolves.toBe(-1);
  });

  it('bounds the snapshot path too, not just set', async () => {
    const cache = new CacheService(configWith(2));

    for (let i = 0; i < 10; i += 1) {
      await cache.getOrSet('proposal', `id-${i}`, () => Promise.resolve(i));
    }

    expect(size(cache)).toBe(2);
  });
});
