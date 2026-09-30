/**
 * `getGenesisParams` from the network's Shelley genesis file.
 *
 * db-sync keeps no genesis constants, only `meta.start_time` (the system
 * start). The file is the one db-sync itself was started with, so the
 * provider reads it when the caller names its path, and checks it against
 * `meta.start_time`: a devnet regenerates its genesis on every run, and a
 * stale file would date every epoch from the wrong start.
 */
import { readFile } from 'node:fs/promises';

import type { GenesisParams } from '@govtool/data-providers/chain-data';

import type { Db } from '../db';
import { internal } from '../errors';
import { toRatio } from '../governance/proposals/ratio';
import { toIso } from '../numbers';

export const META_START_SQL = 'SELECT start_time FROM meta ORDER BY id LIMIT 1';

interface MetaStartRow {
  start_time: Date | string | null;
}

const positiveInt = (value: unknown, what: string): number => {
  if (typeof value !== 'number' || !Number.isSafeInteger(value) || value <= 0) {
    throw internal(`Shelley genesis ${what} is not a positive integer`);
  }
  return value;
};

/**
 * The Shelley genesis JSON → `GenesisParams`. `text` is the raw file, because
 * `maxLovelaceSupply` can exceed a double's exact range and is read off the
 * text rather than the parsed number.
 */
export function mapShelleyGenesis(text: string): GenesisParams {
  let json: Record<string, unknown>;
  try {
    const parsed: unknown = JSON.parse(text);
    if (typeof parsed !== 'object' || parsed === null || Array.isArray(parsed)) throw new Error('not an object');
    json = parsed as Record<string, unknown>;
  } catch {
    throw internal('Shelley genesis is not a JSON object');
  }
  const networkId = json.networkId;
  if (networkId !== 'Mainnet' && networkId !== 'Testnet') {
    throw internal('Shelley genesis networkId is neither Mainnet nor Testnet');
  }
  const systemStart = typeof json.systemStart === 'string' ? Date.parse(json.systemStart) : Number.NaN;
  if (Number.isNaN(systemStart)) throw internal('Shelley genesis systemStart is not a timestamp');
  const slotLength = json.slotLength;
  if (typeof slotLength !== 'number' || !Number.isFinite(slotLength) || slotLength <= 0) {
    throw internal('Shelley genesis slotLength is not a positive number');
  }
  const activeSlotsCoefficient = toRatio(json.activeSlotsCoeff);
  if (!activeSlotsCoefficient || activeSlotsCoefficient.denominator <= 0) {
    throw internal('Shelley genesis activeSlotsCoeff is not a rational');
  }
  const supply = /"maxLovelaceSupply"\s*:\s*(\d+)\s*[,}]/.exec(text)?.[1];
  if (supply === undefined) throw internal('Shelley genesis maxLovelaceSupply is not an integer');
  const networkMagic = json.networkMagic;
  if (typeof networkMagic !== 'number' || !Number.isSafeInteger(networkMagic) || networkMagic < 0) {
    throw internal('Shelley genesis networkMagic is not an integer');
  }
  return {
    networkMagic,
    networkId,
    systemStart: new Date(systemStart).toISOString(),
    epochLength: positiveInt(json.epochLength, 'epochLength'),
    slotLength,
    activeSlotsCoefficient,
    securityParam: positiveInt(json.securityParam, 'securityParam'),
    slotsPerKesPeriod: positiveInt(json.slotsPerKESPeriod, 'slotsPerKESPeriod'),
    maxKesEvolutions: positiveInt(json.maxKESEvolutions, 'maxKESEvolutions'),
    updateQuorum: positiveInt(json.updateQuorum, 'updateQuorum'),
    maxLovelaceSupply: supply.replace(/^0+(?=\d)/, ''),
  };
}

/**
 * Reads the file on every call (it is small, and a devnet rewrites it), then
 * refuses it when its system start is not the database's.
 */
export async function readShelleyGenesis(path: string, db: Db): Promise<GenesisParams> {
  let text: string;
  try {
    text = await readFile(path, 'utf8');
  } catch (cause) {
    // The path is deployment configuration; it stays out of the message.
    throw internal('the configured Shelley genesis file cannot be read', cause);
  }
  const params = mapShelleyGenesis(text);
  const [row] = await db.query<MetaStartRow>(META_START_SQL);
  if (row?.start_time !== null && row?.start_time !== undefined) {
    const dbStart = Date.parse(toIso(row.start_time));
    if (dbStart !== Date.parse(params.systemStart)) {
      throw internal(
        `the Shelley genesis starts at ${params.systemStart} but the database follows a chain started at ${new Date(dbStart).toISOString()}`,
      );
    }
  }
  return params;
}
