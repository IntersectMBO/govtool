import { Inject, Injectable, Optional } from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { ConfigService } from 'src/config/config.service';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { isBareStakeKeyHash, legacyStakeAddress } from './legacy-ids';

/**
 * When each epoch of a network starts: `systemStart + epoch × epochLength`.
 *
 * The legacy SQL computed an action's expiry as
 * `latest_epoch.start_time + (expiration - latest_epoch.no) × epoch length`.
 * On the three public networks every epoch, Byron ones included (21,600 slots
 * of 20 s), has lasted exactly `epochLength` seconds, so the latest epoch's
 * start time is itself `systemStart + no × epochLength` and the two formulas
 * give the same instant. These are genesis constants (`systemStart` in the
 * Byron/Shelley genesis, `epochLength` in the Shelley genesis), not estimates.
 */
export interface EpochSchedule {
  /** Milliseconds since the Unix epoch. */
  systemStartMs: number;
  epochLengthSeconds: number;
}

const EPOCH_SCHEDULES: Readonly<Record<string, EpochSchedule>> = {
  mainnet: {
    systemStartMs: Date.parse('2017-09-23T21:44:51Z'),
    epochLengthSeconds: 432_000,
  },
  preprod: {
    systemStartMs: Date.parse('2022-06-01T00:00:00Z'),
    epochLengthSeconds: 432_000,
  },
  preview: {
    systemStartMs: Date.parse('2022-10-25T00:00:00Z'),
    epochLengthSeconds: 86_400,
  },
};

/** A network's epoch schedule, or `null` for one with no known genesis. */
export function epochScheduleOf(network: string): EpochSchedule | null {
  return Object.prototype.hasOwnProperty.call(EPOCH_SCHEDULES, network)
    ? EPOCH_SCHEDULES[network]
    : null;
}

/**
 * The start of `epoch` in the legacy timestamp format — whole seconds, `Z`,
 * no fraction (`2026-09-24T00:00:00Z`), which is how the Haskell backend
 * rendered db-sync's `timestamp` columns. `null` for an epoch that cannot be
 * placed.
 */
export function epochStartTime(
  schedule: EpochSchedule,
  epoch: number,
): string | null {
  if (!Number.isSafeInteger(epoch) || epoch < 0) {
    return null;
  }
  const ms =
    schedule.systemStartMs + epoch * schedule.epochLengthSeconds * 1000;
  const date = new Date(ms);
  if (Number.isNaN(date.getTime())) {
    return null;
  }
  return date.toISOString().replace(/\.\d{3}Z$/, 'Z');
}

/**
 * Which network the backend is serving, for the two legacy behaviours that
 * depend on it: reading a bare 56-hex stake key hash as a reward address, and
 * dating an epoch the provider reports without a time.
 *
 * db-sync is configured with its network, so that answer needs no call. Any
 * other provider is asked once (`network.getNetworkInfo()`), and the answer is
 * kept: a running backend never changes networks. A failed lookup is not kept,
 * so the next request retries it.
 */
@Injectable()
export class LegacyNetwork {
  private pending: Promise<string> | undefined;

  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    @Optional() private readonly configService?: ConfigService,
  ) {}

  name(): Promise<string> {
    const configured = this.configService?.get().dbSync?.network;
    if (configured !== undefined) {
      return Promise.resolve(configured);
    }
    if (this.pending === undefined) {
      const lookup = this.chain.network
        .getNetworkInfo()
        .then(({ data }) => data.network);
      this.pending = lookup;
      lookup.catch(() => {
        if (this.pending === lookup) {
          this.pending = undefined;
        }
      });
    }
    return this.pending;
  }

  /**
   * `legacyStakeAddress`, with the served network supplied for the one form
   * that needs it. Every other form is validated without a provider call, so
   * a malformed key is a 400 even when the provider is down.
   */
  async stakeAddress(input: string): Promise<string> {
    return isBareStakeKeyHash(input)
      ? legacyStakeAddress(input, await this.name())
      : legacyStakeAddress(input);
  }

  async epochSchedule(): Promise<EpochSchedule | null> {
    return epochScheduleOf(await this.name());
  }
}
