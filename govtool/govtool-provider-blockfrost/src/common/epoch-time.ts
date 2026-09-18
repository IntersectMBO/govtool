import type { EpochStamp } from '@govtool/data-providers/chain-data';

import type { BlockfrostClient } from '../http/client';
import type { BfEpoch } from '../http/types';
import { toIsoFromUnixSeconds } from './numbers';

/**
 * Resolves an epoch number to the wall-clock time it started.
 *
 * Blockfrost dates several things by epoch only — a DRep's `active_epoch`, an
 * action's `enacted_epoch` — while the contract's `EpochStamp` wants both
 * halves where they are known. `/epochs/{n}` has the exact `start_time`, so
 * the other half is one cheap read away.
 *
 * A past epoch's start time never changes, so it is memoised. This is a cache
 * of immutable facts, not of query results: provider-level caching of *data*
 * belongs to the consumer, but re-fetching an epoch boundary that cannot move
 * would just be waste. The map is bounded by the number of epochs a process
 * asks about, and the current epoch is not cached, since its record is still
 * being written.
 */
export class EpochTimeResolver {
  private readonly startTimes = new Map<number, string>();

  constructor(
    private readonly client: BlockfrostClient,
    private readonly currentEpoch: () => Promise<number | undefined>,
  ) {}

  /** An `EpochStamp` carrying the epoch, plus its start time when resolvable. */
  async stamp(epoch: number | null | undefined): Promise<EpochStamp | null> {
    if (epoch === null || epoch === undefined) return null;
    const time = await this.startTime(epoch);
    return time === undefined ? { epoch } : { epoch, time };
  }

  async startTime(epoch: number): Promise<string | undefined> {
    const cached = this.startTimes.get(epoch);
    if (cached !== undefined) return cached;

    const record = await this.client.getOrNull<BfEpoch>(`/epochs/${epoch}`);
    if (record === null) return undefined;

    const iso = toIsoFromUnixSeconds(record.start_time);
    const current = await this.currentEpoch();
    if (current === undefined || epoch < current) {
      this.startTimes.set(epoch, iso);
    }
    return iso;
  }
}
