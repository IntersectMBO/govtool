/**
 * Chain coordinates: the tip, and turning a Koios UNIX timestamp into an
 * epoch where an endpoint (`/drep_updates`, `/pool_votes`) reports only the
 * time.
 *
 * The epoch arithmetic rests on two IMMUTABLE facts — the network's epoch
 * duration from genesis, and when one known epoch started — so they are read
 * once and kept. That is not caching in SPEC.md §3.6's sense: neither value
 * can ever change, so neither can go stale. The tip is read fresh every time.
 */
import type { EpochStamp, NetworkId } from '@govtool/data-providers/chain-data';

import { internal, invalidInput } from './errors';
import type { KoiosHttp } from './http';
import { toIso } from './numbers';
import type { EpochInfoRow, GenesisRow, TipRow } from './rows';

/** Network magic of the public networks, to catch a provider pointed at the wrong one. */
const MAGIC: Record<string, string> = { mainnet: '764824073', preprod: '1', preview: '2' };

export interface Clock {
  /** Seconds per epoch; the same in every era on the public networks. */
  epochSeconds: number;
  anchorEpoch: number;
  anchorStart: number;
}

export interface Chain {
  tip(): Promise<TipRow>;
  /** The configured network, checked once against Koios' genesis. */
  genesis(): Promise<GenesisRow>;
  clock(): Promise<Clock>;
  /** An `EpochStamp` for a block time, epoch computed from the clock. */
  stampAt(unixSeconds: number, extra?: { slot?: number | null; block?: number | null }): Promise<EpochStamp>;
  /** The start of `epoch`, ISO-8601. */
  epochStart(epoch: number): Promise<string>;
}

export function epochAt(clock: Clock, unixSeconds: number): number {
  return clock.anchorEpoch + Math.floor((unixSeconds - clock.anchorStart) / clock.epochSeconds);
}

export function createChain(http: KoiosHttp, network: NetworkId): Chain {
  let genesisMemo: Promise<GenesisRow> | undefined;
  let clockMemo: Promise<Clock> | undefined;

  const tip = async (): Promise<TipRow> => {
    const { rows } = await http.get<TipRow>('tip');
    const row = rows[0];
    if (!row) throw internal('Koios returned no tip');
    return row;
  };

  const genesis = () => {
    genesisMemo ??= (async () => {
      const { rows } = await http.get<GenesisRow>('genesis');
      const row = rows[0];
      if (!row) throw internal('Koios returned no genesis');
      const expected = MAGIC[network];
      if (expected !== undefined && row.networkmagic !== expected) {
        // Stake addresses and ids would be read for the wrong network: a configuration error, not data.
        throw internal(`provider is configured for ${network} but Koios serves network magic ${row.networkmagic}`);
      }
      return row;
    })().catch((error) => {
      genesisMemo = undefined;
      throw error;
    });
    return genesisMemo;
  };

  const clock = () => {
    clockMemo ??= (async () => {
      const [g, t] = await Promise.all([genesis(), tip()]);
      const epochSeconds = Number(g.epochlength) * Number(g.slotlength);
      if (!Number.isFinite(epochSeconds) || epochSeconds <= 0) throw internal('Koios genesis has no epoch length');
      const { rows } = await http.get<EpochInfoRow>(
        'epoch_info',
        { _epoch_no: t.epoch_no, _include_next_epoch: false },
        { select: 'epoch_no,start_time,end_time' },
      );
      const e = rows[0];
      if (!e) throw internal('Koios has no epoch_info for the tip epoch', { epoch: t.epoch_no });
      return { epochSeconds, anchorEpoch: e.epoch_no, anchorStart: e.start_time };
    })().catch((error) => {
      clockMemo = undefined;
      throw error;
    });
    return clockMemo;
  };

  return {
    tip,
    genesis,
    clock,
    async stampAt(unixSeconds, extra = {}) {
      const c = await clock();
      return {
        epoch: epochAt(c, unixSeconds),
        ...(extra.slot === null || extra.slot === undefined ? {} : { slot: extra.slot }),
        ...(extra.block === null || extra.block === undefined ? {} : { block: extra.block }),
        time: toIso(unixSeconds),
      };
    },
    async epochStart(epoch) {
      const c = await clock();
      return toIso(c.anchorStart + (epoch - c.anchorEpoch) * c.epochSeconds);
    },
  };
}

/** Validate an optional `epoch` argument: a non-negative integer, or absent. */
export function parseEpoch(value: unknown): number | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0) {
    throw invalidInput('epoch must be a non-negative integer', { epoch: value });
  }
  return value;
}
