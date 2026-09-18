import { Logger } from '@nestjs/common';
import { CacheWarmerService } from './cache-warmer.service';
import { DbService } from '../db/db.service';
import { DRepService } from '../drep/drep.service';
import { ProposalService } from '../proposal/proposal.service';

describe('cache warmer recovery', () => {
  it('survives startup and interval query failures, then resumes warming', async () => {
    jest.useFakeTimers();
    jest.spyOn(Logger.prototype, 'error').mockImplementation(() => {});
    jest.spyOn(Logger.prototype, 'log').mockImplementation(() => {});
    const query = jest.fn().mockRejectedValue(new Error('DB unavailable'));
    const warm = jest.fn().mockResolvedValue(undefined);
    const service = new CacheWarmerService(
      { query } as unknown as DbService,
      { warmDefaultListSnapshot: warm } as unknown as DRepService,
      { warmActiveProposalSnapshot: warm } as unknown as ProposalService,
    );
    try {
      await expect(service.onModuleInit()).resolves.toBeUndefined();
      await jest.advanceTimersByTimeAsync(20_000);
      expect(query).toHaveBeenCalledTimes(2);
      expect(warm).not.toHaveBeenCalled();
      query.mockResolvedValue({ rows: [{ block_no: 42 }] });
      await jest.advanceTimersByTimeAsync(20_000);
      expect(warm).toHaveBeenCalledTimes(2);
      await jest.advanceTimersByTimeAsync(20_000);
      expect(warm).toHaveBeenCalledTimes(2);
    } finally {
      service.onModuleDestroy();
      jest.useRealTimers();
      jest.restoreAllMocks();
    }
  });
});
