import { CacheService } from './cache.service';
import { ConfigService } from '../config/config.service';

describe('bounded cache', () => {
  let cache: CacheService;
  beforeEach(() => {
    cache = new CacheService({
      get: () => ({ cacheMaxEntries: 2, cacheDurationSeconds: 20 }),
    } as ConfigService);
  });
  it('evicts the least recently used entry across all insertion paths', async () => {
    cache.set('n', 'a', 1);
    await cache.getOrSet('n', 'b', async () => 2);
    await cache.getOrSet('n', 'a', async () => 99);
    await cache.refresh('n', 'c', async () => 3);
    const miss = jest.fn(async () => 4);
    expect(await cache.getOrSet('n', 'a', miss)).toBe(1);
    expect(await cache.getOrSet('n', 'b', miss)).toBe(4);
    expect(miss).toHaveBeenCalledTimes(1);
  });
  it('serves stale data during refresh and does not resurrect evicted entries', async () => {
    cache.set('n', 'a', 1, -1);
    let complete!: (value: number) => void;
    const action = jest.fn(
      () =>
        new Promise<number>((resolve) => {
          complete = resolve;
        }),
    );
    expect(await cache.getOrSetStaleWhileRevalidate('n', 'a', action)).toBe(1);
    expect(await cache.getOrSetStaleWhileRevalidate('n', 'a', action)).toBe(1);
    expect(action).toHaveBeenCalledTimes(1);
    cache.set('n', 'b', 2);
    cache.set('n', 'c', 3);
    complete(9);
    await Promise.resolve();
    await Promise.resolve();
    expect(await cache.getOrSet('n', 'a', async () => 4)).toBe(4);
  });
  it('shares cold snapshot requests and allows retry after rejection', async () => {
    const action = jest.fn(async () => 3);
    expect(
      await Promise.all([
        cache.getOrSetStaleWhileRevalidate('n', 'a', action),
        cache.getOrSetStaleWhileRevalidate('n', 'a', action),
      ]),
    ).toEqual([3, 3]);
    expect(action).toHaveBeenCalledTimes(1);
    await expect(
      cache.getOrSet('n', 'bad', async () => {
        throw new Error('failed');
      }),
    ).rejects.toThrow('failed');
    expect(await cache.getOrSet('n', 'bad', async () => 5)).toBe(5);
  });
});
