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

/** Reaches the private partitions, which is the only way to observe eviction. */
function size(cache: CacheService): number {
  const { caches } = cache as unknown as {
    caches: Map<string, Map<string, unknown>>;
  };
  return [...caches.values()].reduce((sum, part) => sum + part.size, 0);
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

  it('keeps one namespace from evicting another', async () => {
    const cache = new CacheService(configWith(2));

    cache.set('proposal', 'kept', 1);
    for (let i = 0; i < 10; i += 1) {
      cache.set('drepInfo', `drep-${i}`, i);
    }

    await expect(
      cache.getOrSet('proposal', 'kept', () => Promise.resolve(-1)),
    ).resolves.toBe(1);
  });

  it('bounds the snapshot path too, not just set', async () => {
    const cache = new CacheService(configWith(2));

    for (let i = 0; i < 10; i += 1) {
      await cache.getOrSet('proposal', `id-${i}`, () => Promise.resolve(i));
    }

    expect(size(cache)).toBe(2);
  });
});

describe('CacheService.noteBlock', () => {
  const read = (cache: CacheService, namespace: string, key: string) =>
    cache.getOrSet(namespace, key, () => Promise.resolve('fresh'));

  it("drops a wallet's own chain state when a newer block arrives", async () => {
    const cache = new CacheService(configWith(10));
    cache.noteBlock(100);
    cache.set('drepVotes', 'drep1', 'before the vote');
    cache.set('drepInfo', 'drep1', 'before the registration');

    cache.noteBlock(101);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe('fresh');
    await expect(read(cache, 'drepInfo', 'drep1')).resolves.toBe('fresh');
  });

  it('keeps the shared snapshots, which the warmer refreshes itself', async () => {
    const cache = new CacheService(configWith(10));
    cache.noteBlock(100);
    cache.set('drepListSnapshot', '', 'snapshot');

    cache.noteBlock(101);

    await expect(read(cache, 'drepListSnapshot', '')).resolves.toBe('snapshot');
  });

  it('clears once per block, however often the same block is reported', async () => {
    // /transaction/status reports blocks for any caller, so a repeated or an
    // older block must not flush the caches again.
    const cache = new CacheService(configWith(10));
    cache.noteBlock(101);
    cache.set('drepVotes', 'drep1', 'cached');

    cache.noteBlock(101);
    cache.noteBlock(99);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe('cached');
  });
});

describe('CacheService.noteTip', () => {
  const read = (cache: CacheService, namespace: string, key: string) =>
    cache.getOrSet(namespace, key, () => Promise.resolve('fresh'));

  it('clears once per tip, however often the warmer reads it', async () => {
    const cache = new CacheService(configWith(10));
    cache.noteTip(100);
    cache.set('drepVotes', 'drep1', 'cached');

    cache.noteTip(100);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe('cached');
  });

  it('clears on a rollback, and on each block after it', async () => {
    const cache = new CacheService(configWith(10));
    cache.noteTip(100);
    cache.set('drepVotes', 'drep1', 'from the dropped block');

    cache.noteTip(99);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe('fresh');

    // The chain grows back over a height it already reported once.
    cache.set('drepVotes', 'drep1', 'before block 100 again');
    cache.noteTip(100);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe('fresh');
  });

  it('clears again after a chain reset, long before the old height', async () => {
    const cache = new CacheService(configWith(10));
    cache.noteTip(50_000);

    cache.noteTip(3);
    cache.set('drepInfo', 'drep1', 'before the registration');
    cache.noteTip(4);

    await expect(read(cache, 'drepInfo', 'drep1')).resolves.toBe('fresh');
  });

  it('lets a transaction move the mark forward but never back', async () => {
    const cache = new CacheService(configWith(10));
    cache.noteTip(100);
    cache.noteBlock(101);
    cache.set('drepVotes', 'drep1', 'after the vote');

    // An old transaction cannot lower the mark, so 101 does not clear twice.
    cache.noteBlock(5);
    cache.noteBlock(101);

    await expect(read(cache, 'drepVotes', 'drep1')).resolves.toBe(
      'after the vote',
    );
  });
});
