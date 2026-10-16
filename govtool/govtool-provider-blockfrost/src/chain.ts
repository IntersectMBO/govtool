/**
 * Chain coordinates: the current epoch, dating a transaction, and a
 * transaction's CBOR — each loaded at most once per call (see `Session`).
 *
 * Blockfrost dates a transaction by block time and slot but not by epoch.
 * Every Shelley-and-later epoch has the same length, so the epoch of a time is
 * exact arithmetic from the current epoch's start and end; governance
 * transactions are all Conway-era, far past the Byron boundary where that
 * stops holding.
 */
import type { EpochStamp, TxRef } from '@govtool/data-providers/chain-data';

import { decodeCbor, type Cbor } from './cbor';
import type { Session } from './context';
import { internal, invalidInput } from './errors';
import { isoFromUnix } from './numbers';

export interface BfEpoch {
  epoch: number;
  start_time: number;
  end_time: number;
  active_stake: string | null;
}

export interface BfTx {
  hash: string;
  block_height: number;
  block_time: number;
  slot: number;
  index: number;
}

export interface Clock {
  /** The current epoch. */
  epoch: number;
  latest: BfEpoch;
  epochOf(unixSeconds: number): number;
}

export function clockFrom(latest: BfEpoch): Clock {
  const length = latest.end_time - latest.start_time;
  if (!Number.isInteger(length) || length <= 0) throw internal('Blockfrost reported an epoch with no length');
  return {
    epoch: latest.epoch,
    latest,
    epochOf: (t) => latest.epoch + Math.floor((t - latest.start_time) / length),
  };
}

export const loadClock = (s: Session): Promise<Clock> =>
  s.once('clock', async () => clockFrom(await s.http.get<BfEpoch>('/epochs/latest')));

/** A transaction's block, or `null` when Blockfrost has not seen it. */
export const loadTx = (s: Session, hash: string): Promise<BfTx | null> =>
  s.once(`tx:${hash}`, () => s.http.getOrNull<BfTx>(`/txs/${hash}`));

export async function requireTx(s: Session, hash: string): Promise<BfTx> {
  const tx = await loadTx(s, hash);
  if (!tx) throw internal('Blockfrost has no record of a transaction it referenced', { txHash: hash });
  return tx;
}

export const loadTxCbor = (s: Session, hash: string): Promise<Cbor> =>
  s.once(`cbor:${hash}`, async () => {
    const { cbor } = await s.http.get<{ cbor: string }>(`/txs/${hash}/cbor`);
    return decodeCbor(cbor);
  });

export function stampOf(tx: BfTx, clock: Clock): EpochStamp {
  return { epoch: clock.epochOf(tx.block_time), slot: tx.slot, block: tx.block_height, time: isoFromUnix(tx.block_time) };
}

/** Date a transaction, for a `TxRef.at` or a `RegistrationEvent.at`. */
export async function txStamp(s: Session, hash: string): Promise<EpochStamp> {
  const [tx, clock] = await Promise.all([requireTx(s, hash), loadClock(s)]);
  return stampOf(tx, clock);
}

export function txRefOf(hash: string, index: number, at?: EpochStamp): TxRef {
  return { txHash: hash, index, ...(at?.block === undefined ? {} : { block: at.block }), ...(at ? { at } : {}) };
}

/** Validate an optional `epoch` argument: a non-negative integer, or absent. */
export function parseEpoch(value: unknown): number | undefined {
  if (value === undefined) return undefined;
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value < 0) {
    throw invalidInput('epoch must be a non-negative integer', { epoch: value });
  }
  return value;
}
