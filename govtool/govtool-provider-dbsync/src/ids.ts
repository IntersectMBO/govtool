/**
 * Identifiers, strictly per SPEC.md §3.1: one bech32 form per entity, decoded
 * and validated, never string-matched. CIP-105 DRep ids share the `drep1`
 * prefix with CIP-129 ones and differ only by the header byte, so anything
 * without a valid CIP-129 header is rejected rather than guessed at.
 */
import { bech32 } from 'bech32';
import type { NetworkId } from '@govtool/data-providers/chain-data';

import { invalidInput } from './errors';

/** Cardano ids exceed bech32's 90-character default. */
const LIMIT = 1023;
const HASH_BYTES = 28;
const TX_HASH_BYTES = 32;

/** CIP-129 header bytes: high nibble is the key type, low nibble 2 = key hash, 3 = script hash. */
const CIP129 = {
  cc_hot: 0x0,
  cc_cold: 0x1,
  drep: 0x2,
} as const;
type Cip129Kind = keyof typeof CIP129;

function decode(value: string): { prefix: string; bytes: Buffer } | undefined {
  try {
    const decoded = bech32.decode(value, LIMIT);
    return { prefix: decoded.prefix, bytes: Buffer.from(bech32.fromWords(decoded.words)) };
  } catch {
    return undefined;
  }
}

const encode = (prefix: string, bytes: Buffer) => bech32.encode(prefix, bech32.toWords(bytes), LIMIT);

export interface Credential {
  /** Lowercase hex of the 28-byte credential hash, as db-sync stores it. */
  hash: string;
  isScript: boolean;
}

function encodeCip129(kind: Cip129Kind, hashHex: string, isScript: boolean): string {
  const header = (CIP129[kind] << 4) | (isScript ? 0x3 : 0x2);
  return encode(kind, Buffer.concat([Buffer.from([header]), Buffer.from(hashHex, 'hex')]));
}

function decodeCip129(kind: Cip129Kind, id: string, label: string): Credential {
  const decoded = decode(id);
  if (!decoded || decoded.prefix !== kind || decoded.bytes.length !== HASH_BYTES + 1) {
    throw invalidInput(`Not a CIP-129 ${label} id`, { id });
  }
  const header = decoded.bytes[0]!;
  const typeNibble = header & 0x0f;
  if (header >> 4 !== CIP129[kind] || (typeNibble !== 0x2 && typeNibble !== 0x3)) {
    throw invalidInput(`Not a CIP-129 ${label} id`, { id });
  }
  return { hash: decoded.bytes.subarray(1).toString('hex'), isScript: typeNibble === 0x3 };
}

export const encodeDRepId = (hash: string, isScript: boolean) => encodeCip129('drep', hash, isScript);
export const decodeDRepId = (id: string) => decodeCip129('drep', id, 'DRep');

export const encodeCommitteeColdId = (hash: string, isScript: boolean) => encodeCip129('cc_cold', hash, isScript);
export const decodeCommitteeColdId = (id: string) => decodeCip129('cc_cold', id, 'committee cold credential');

export const encodeCommitteeHotId = (hash: string, isScript: boolean) => encodeCip129('cc_hot', hash, isScript);
export const decodeCommitteeHotId = (id: string) => decodeCip129('cc_hot', id, 'committee hot credential');

export interface GovActionIdParts {
  txHash: string;
  index: number;
}

/** CIP-129 governance action id: the 32-byte tx hash followed by the index byte. */
export function encodeGovActionId(txHash: string, index: number): string {
  if (!Number.isInteger(index) || index < 0 || index > 255) {
    throw invalidInput('Governance action index out of range for CIP-129', { txHash, index });
  }
  return encode('gov_action', Buffer.concat([Buffer.from(txHash, 'hex'), Buffer.from([index])]));
}

export function decodeGovActionId(id: string): GovActionIdParts {
  const decoded = decode(id);
  if (!decoded || decoded.prefix !== 'gov_action' || decoded.bytes.length !== TX_HASH_BYTES + 1) {
    throw invalidInput('Not a CIP-129 governance action id', { id });
  }
  return { txHash: decoded.bytes.subarray(0, TX_HASH_BYTES).toString('hex'), index: decoded.bytes[TX_HASH_BYTES]! };
}

export const encodePoolId = (hash: string) => encode('pool', Buffer.from(hash, 'hex'));

export function decodePoolId(id: string): string {
  const decoded = decode(id);
  if (!decoded || decoded.prefix !== 'pool' || decoded.bytes.length !== HASH_BYTES) {
    throw invalidInput('Not a pool1 id', { id });
  }
  return decoded.bytes.toString('hex');
}

const isMainnet = (network: NetworkId) => network === 'mainnet';
const stakePrefix = (network: NetworkId) => (isMainnet(network) ? 'stake' : 'stake_test');

/** A reward address from its credential: header 0xe_ for key, 0xf_ for script, low nibble the network id. */
export function encodeStakeAddress(hash: string, isScript: boolean, network: NetworkId): string {
  const header = (isScript ? 0xf0 : 0xe0) | (isMainnet(network) ? 1 : 0);
  return encode(stakePrefix(network), Buffer.concat([Buffer.from([header]), Buffer.from(hash, 'hex')]));
}

/** A reward address for this provider's network, decoded to its credential. */
export function decodeStakeAddress(address: string, network: NetworkId): Credential {
  const decoded = decode(address);
  if (!decoded || decoded.prefix !== stakePrefix(network) || decoded.bytes.length !== HASH_BYTES + 1) {
    throw invalidInput(`Not a ${stakePrefix(network)}1 address`, { stakeAddress: address });
  }
  const header = decoded.bytes[0]!;
  const kind = header & 0xf0;
  if ((kind !== 0xe0 && kind !== 0xf0) || (header & 0x0f) !== (isMainnet(network) ? 1 : 0)) {
    throw invalidInput(`Not a ${stakePrefix(network)}1 address`, { stakeAddress: address });
  }
  return { hash: decoded.bytes.subarray(1).toString('hex'), isScript: kind === 0xf0 };
}

/** db-sync `stake_address.view` for a known address, used to bind queries. */
export const isHex = (value: string, bytes?: number) =>
  /^[0-9a-fA-F]*$/.test(value) && value.length % 2 === 0 && (bytes === undefined || value.length === bytes * 2);
