// Real CIP-8 / CIP-30 signData results from libcardano keys, for unit and
// e2e tests. Nothing here is mocked: the backend verifies these exactly as it
// verifies a wallet's.

import { blake, cip8Sign, Ed25519Key } from 'libcardano';

export interface SignOptions {
  /** Protected `address` header bytes. Default: the key's testnet reward address. */
  address?: Buffer;
  /** Sign blake2b-224(payload) and set the unprotected `hashed` header. */
  hashed?: boolean;
  /** Override COSE_Key label 3 (alg). */
  alg?: number;
  /** Override COSE_Key label 1 (kty). */
  keyType?: number;
  /** Flip one bit of the signature. */
  tamper?: boolean;
}

export interface SignedData {
  signature: string;
  key: string;
}

export function newKey(): Ed25519Key {
  return Ed25519Key.generate();
}

/** 29-byte key-hash reward address: header e0 (testnet) or e1 (mainnet) + pkh. */
export function rewardAddress(key: Ed25519Key, networkId: 0 | 1 = 0): Buffer {
  return Buffer.concat([Buffer.from([0xe0 | networkId]), Buffer.from(key.pkh)]);
}

/** A base address (type 0): payment key hash + stake key hash. */
export function baseAddress(payment: Ed25519Key, stake: Ed25519Key, networkId: 0 | 1 = 0): Buffer {
  return Buffer.concat([Buffer.from([0x00 | networkId]), Buffer.from(payment.pkh), Buffer.from(stake.pkh)]);
}

/** The stake-login identifier of a key (58 hex). */
export function stakeIdentifier(key: Ed25519Key, networkId: 0 | 1 = 0): string {
  return rewardAddress(key, networkId).toString('hex');
}

/** The DRep-login identifier of a key (56 hex key hash). */
export function drepIdentifier(key: Ed25519Key): string {
  return Buffer.from(key.pkh).toString('hex');
}

export async function signCip8(
  key: Ed25519Key,
  payload: string | Buffer,
  opts: SignOptions = {},
): Promise<SignedData> {
  const raw = typeof payload === 'string' ? Buffer.from(payload, 'utf8') : payload;
  const toSign = opts.hashed ? blake.hash28(raw) : raw;
  const res = await cip8Sign(opts.address ?? rewardAddress(key), key, toSign);
  if (opts.hashed) res.coseSignature.headers.unprotectedHeaders.setHeader('hashed', true);
  if (opts.alg !== undefined) res.key.algorithmId = opts.alg;
  if (opts.keyType !== undefined) res.key.keyType = opts.keyType;
  if (opts.tamper) res.coseSignature.rawSignature[0] ^= 0x01;
  return {
    signature: Buffer.from(res.coseSignature.toBytes()).toString('hex'),
    key: Buffer.from(res.key.toBytes()).toString('hex'),
  };
}
