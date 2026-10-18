import { Inject, Injectable, Logger, Optional } from '@nestjs/common';
import type {
  ChainDataApiV1,
  GenesisParams,
} from '@govtool/data-providers/chain-data';

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
 *
 * Any other network (a local devnet) regenerates its genesis on every run, so
 * its schedule is read from the provider's `getGenesisParams` rather than
 * kept here. Milliseconds, because a devnet's slots can be shorter than a
 * second (300 slots of 0.2 s).
 */
export interface EpochSchedule {
  /** Milliseconds since the Unix epoch. */
  systemStartMs: number;
  epochLengthMs: number;
}

const EPOCH_SCHEDULES: Readonly<Record<string, EpochSchedule>> = {
  mainnet: {
    systemStartMs: Date.parse('2017-09-23T21:44:51Z'),
    epochLengthMs: 432_000_000,
  },
  preprod: {
    systemStartMs: Date.parse('2022-06-01T00:00:00Z'),
    epochLengthMs: 432_000_000,
  },
  preview: {
    systemStartMs: Date.parse('2022-10-25T00:00:00Z'),
    epochLengthMs: 86_400_000,
  },
};

/** A public network's epoch schedule, or `null` for one with no known genesis. */
export function epochScheduleOf(network: string): EpochSchedule | null {
  return Object.prototype.hasOwnProperty.call(EPOCH_SCHEDULES, network)
    ? EPOCH_SCHEDULES[network]
    : null;
}

/**
 * The schedule a Shelley genesis implies: `epochLength` slots of `slotLength`
 * seconds from `systemStart`. Right for a network that began in Shelley or
 * later, which is every devnet; a network that began in Byron is in
 * `EPOCH_SCHEDULES` instead. `null` for constants that do not make one.
 */
export function epochScheduleFromGenesis(
  genesis: Pick<GenesisParams, 'systemStart' | 'epochLength' | 'slotLength'>,
): EpochSchedule | null {
  const systemStartMs = Date.parse(genesis.systemStart);
  // Rounded to whole ms: 300 × 0.2 s is 60.00000000000001 s in a double.
  const epochLengthMs = Math.round(
    genesis.epochLength * genesis.slotLength * 1000,
  );
  if (
    Number.isNaN(systemStartMs) ||
    !Number.isSafeInteger(epochLengthMs) ||
    epochLengthMs <= 0
  ) {
    return null;
  }
  return { systemStartMs, epochLengthMs };
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
  const ms = schedule.systemStartMs + epoch * schedule.epochLengthMs;
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
  private readonly logger = new Logger(LegacyNetwork.name);
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

  /**
   * A public network's schedule is a constant. Any other is asked of the
   * provider's genesis constants on every call, since a devnet's change
   * between runs; a provider without them, or a failed read, dates nothing
   * (`null`) rather than failing the route that wanted a date.
   */
  async epochSchedule(): Promise<EpochSchedule | null> {
    const known = epochScheduleOf(await this.name());
    if (known !== null) {
      return known;
    }
    const network = this.chain.network;
    if (network.getGenesisParams === undefined) {
      return null;
    }
    try {
      const { data } = await network.getGenesisParams();
      return epochScheduleFromGenesis(data);
    } catch (error) {
      this.logger.warn(
        `no epoch schedule: genesis parameters unavailable (${error instanceof Error ? error.message : String(error)})`,
      );
      return null;
    }
  }
}
