import { Logger } from '@nestjs/common';
import { AdaHolderService } from './ada-holder.service';
import { CacheService } from '../cache/cache.service';
import { ConfigService } from '../config/config.service';
import type { DbService } from '../db/db.service';
import type { SqlService } from '../sql/sq.service';

describe('AdaHolderService.getVotingPower', () => {
  const stakeKey = 'e0aa';
  let query: jest.Mock;
  let service: AdaHolderService;
  let errorSpy: jest.SpyInstance;

  beforeEach(() => {
    query = jest.fn();
    const cache = new CacheService({
      get: () => ({ cacheMaxEntries: 16, cacheDurationSeconds: 20 }),
    } as ConfigService);
    service = new AdaHolderService(
      { query } as unknown as DbService,
      { load: () => 'SELECT 1' } as unknown as SqlService,
      cache,
    );
    errorSpy = jest
      .spyOn(Logger.prototype, 'error')
      .mockImplementation(() => undefined);
    jest.spyOn(Logger.prototype, 'warn').mockImplementation(() => undefined);
  });

  afterEach(() => jest.restoreAllMocks());

  it('logs database failures, returns 0 and does not cache the fallback', async () => {
    query.mockRejectedValueOnce(new Error('connection terminated'));
    query.mockResolvedValueOnce({ rows: [{ total_balance: '42' }] });

    expect(await service.getVotingPower(stakeKey)).toBe(0);
    expect(errorSpy).toHaveBeenCalledWith(
      expect.stringContaining(stakeKey),
      expect.stringContaining('connection terminated'),
    );
    expect(await service.getVotingPower(stakeKey)).toBe(42);
    expect(query).toHaveBeenCalledTimes(2);
  });

  it('returns 0 when the stake key has no rows', async () => {
    query.mockResolvedValueOnce({ rows: [] });
    expect(await service.getVotingPower(stakeKey)).toBe(0);
  });

  it('rejects non-hex stake keys before querying', async () => {
    await expect(service.getVotingPower('zz')).rejects.toThrow();
    expect(query).not.toHaveBeenCalled();
  });
});
