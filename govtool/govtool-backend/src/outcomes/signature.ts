import { createPublicKey, verify as verifySignature } from 'node:crypto';
import * as blake from 'blakejs';
import * as jsonld from 'jsonld';

import type { SignatureVerificationResult } from './outcomes.type';

/**
 * CIP-100 author witness verification for the outcomes UI.
 *
 * The signed message is the blake2b-256 hash of the document's `@context` and
 * `body`, canonicalised with URDNA2015. Two witness algorithms exist:
 * `ed25519` (a raw signature over that hash) and `CIP-0008` (a COSE_Sign1
 * whose payload is that hash).
 *
 * The document is fetched by the caller through the backend's guarded
 * metadata fetch. A remote `@context` is resolved, as the outcomes service
 * did, but only through the `ContextLoader` the caller passes, which is that
 * same guarded fetch; with no loader a remote context is refused. The policy
 * for which contexts to fetch and how long to keep them is #4255.
 */

export class SignatureInputError extends Error {}

const HEX = /^[0-9a-fA-F]*$/;

function hexBytes(value: unknown, what: string, maxBytes: number): Buffer {
  if (typeof value !== 'string')
    throw new SignatureInputError(`${what} is missing`);
  const text = value.trim().toLowerCase();
  if (text.length === 0 || text.length % 2 !== 0 || !HEX.test(text)) {
    throw new SignatureInputError(`${what} is not a hex string`);
  }
  if (text.length / 2 > maxBytes)
    throw new SignatureInputError(`${what} is too long`);
  return Buffer.from(text, 'hex');
}

/* ------------------------------------------------------------------------- */
/* A minimal CBOR codec: the definite-length subset COSE uses.                */
/* ------------------------------------------------------------------------- */

type Cbor =
  | number
  | bigint
  | string
  | boolean
  | null
  | undefined
  | Buffer
  | Cbor[]
  | Map<Cbor, Cbor>;

const MAX_DEPTH = 16;

export function decodeCbor(bytes: Buffer): Cbor {
  let offset = 0;
  const need = (n: number) => {
    if (offset + n > bytes.length)
      throw new SignatureInputError('truncated CBOR');
  };
  const readLength = (info: number): number => {
    if (info < 24) return info;
    const size =
      info === 24 ? 1 : info === 25 ? 2 : info === 26 ? 4 : info === 27 ? 8 : 0;
    if (size === 0) throw new SignatureInputError('unsupported CBOR length');
    need(size);
    let value = 0n;
    for (let i = 0; i < size; i++)
      value = (value << 8n) | BigInt(bytes[offset + i]);
    offset += size;
    if (value > BigInt(Number.MAX_SAFE_INTEGER))
      throw new SignatureInputError('CBOR length too large');
    return Number(value);
  };
  const item = (depth: number): Cbor => {
    if (depth > MAX_DEPTH)
      throw new SignatureInputError('CBOR nested too deeply');
    need(1);
    const initial = bytes[offset++];
    const major = initial >> 5;
    const info = initial & 0x1f;
    switch (major) {
      case 0:
        return readLength(info);
      case 1:
        return -1 - readLength(info);
      case 2:
      case 3: {
        const length = readLength(info);
        need(length);
        const chunk = bytes.subarray(offset, offset + length);
        offset += length;
        return major === 2 ? Buffer.from(chunk) : chunk.toString('utf8');
      }
      case 4: {
        const length = readLength(info);
        if (length > bytes.length)
          throw new SignatureInputError('CBOR array too long');
        return Array.from({ length }, () => item(depth + 1));
      }
      case 5: {
        const length = readLength(info);
        if (length > bytes.length)
          throw new SignatureInputError('CBOR map too long');
        const map = new Map<Cbor, Cbor>();
        for (let i = 0; i < length; i++) {
          const key = item(depth + 1);
          map.set(key, item(depth + 1));
        }
        return map;
      }
      case 6:
        readLength(info);
        return item(depth + 1);
      default:
        if (info === 20) return false;
        if (info === 21) return true;
        if (info === 22) return null;
        if (info === 23) return undefined;
        throw new SignatureInputError('unsupported CBOR simple value');
    }
  };
  const value = item(0);
  if (offset !== bytes.length)
    throw new SignatureInputError('trailing bytes after CBOR item');
  return value;
}

function head(major: number, length: number): Buffer {
  if (length < 24) return Buffer.from([(major << 5) | length]);
  if (length < 0x100) return Buffer.from([(major << 5) | 24, length]);
  if (length < 0x10000)
    return Buffer.from([(major << 5) | 25, length >> 8, length & 0xff]);
  const out = Buffer.alloc(5);
  out[0] = (major << 5) | 26;
  out.writeUInt32BE(length, 1);
  return out;
}

/** Encodes an array of text and byte strings: all a Sig_structure holds. */
export function encodeCborArray(items: readonly (string | Buffer)[]): Buffer {
  return Buffer.concat([
    head(4, items.length),
    ...items.map((value) => {
      const bytes =
        typeof value === 'string' ? Buffer.from(value, 'utf8') : value;
      return Buffer.concat([
        head(typeof value === 'string' ? 3 : 2, bytes.length),
        bytes,
      ]);
    }),
  ]);
}

/* ------------------------------------------------------------------------- */
/* Ed25519 and COSE                                                           */
/* ------------------------------------------------------------------------- */

function ed25519Verify(
  publicKey: Buffer,
  message: Buffer | Uint8Array,
  signature: Buffer,
): boolean {
  if (publicKey.length !== 32)
    throw new SignatureInputError('Ed25519 public key must be 32 bytes');
  if (signature.length !== 64)
    throw new SignatureInputError('Ed25519 signature must be 64 bytes');
  const key = createPublicKey({
    key: { kty: 'OKP', crv: 'Ed25519', x: publicKey.toString('base64url') },
    format: 'jwk',
  });
  return verifySignature(null, message, key, signature);
}

/**
 * A COSE_Key's `-2` (x) entry, or the input itself as a raw 32-byte vkey.
 * The key must be an Ed25519 OKP key, checked as the outcomes service did.
 */
function coseKeyBytes(publicKey: Buffer): Buffer {
  if (publicKey.length === 32) return publicKey;
  const key = decodeCbor(publicKey);
  if (!(key instanceof Map) || key.size < 4) {
    throw new SignatureInputError(
      'COSE_Key is not valid. It must be a map with at least 4 entries: kty,alg,crv,x.',
    );
  }
  if (key.get(1) !== 1)
    throw new SignatureInputError(
      'COSE_Key map label "1" (kty) is not "1" (OKP)',
    );
  if (key.get(3) !== -8)
    throw new SignatureInputError(
      'COSE_Key map label "3" (alg) is not "-8" (EdDSA)',
    );
  if (key.get(-1) !== 6)
    throw new SignatureInputError(
      'COSE_Key map label "-1" (crv) is not "6" (Ed25519)',
    );
  const x = key.get(-2);
  if (!Buffer.isBuffer(x))
    throw new SignatureInputError('COSE_Key public key is missing');
  return x;
}

export function verifyCip8(
  publicKeyHex: unknown,
  coseSign1Hex: unknown,
  messageHash: Uint8Array,
): SignatureVerificationResult {
  const publicKey = coseKeyBytes(hexBytes(publicKeyHex, 'publicKey', 1024));
  const sign1 = decodeCbor(hexBytes(coseSign1Hex, 'signature', 16 * 1024));
  if (!Array.isArray(sign1) || sign1.length !== 4) {
    throw new SignatureInputError('COSE_Sign1 is not valid');
  }
  const [protectedHeader, unprotectedHeader, payload, signature] = sign1;
  if (!Buffer.isBuffer(protectedHeader))
    throw new SignatureInputError('Protected header is not a byte array');
  const headers = decodeCbor(protectedHeader);
  if (!(headers instanceof Map))
    throw new SignatureInputError('Protected header is not a map');
  if (headers.get(1) !== -8) {
    throw new SignatureInputError(
      'Protected header map label "1" (alg) is not "-8" (EdDSA)',
    );
  }
  if (!headers.has('address')) {
    throw new SignatureInputError(
      'Protected header map label "address" is missing',
    );
  }
  if (!(unprotectedHeader instanceof Map))
    throw new SignatureInputError('Unprotected header is not a map');
  if (!Buffer.isBuffer(payload))
    throw new SignatureInputError('Payload is not a byte array');
  if (!Buffer.isBuffer(signature))
    throw new SignatureInputError('Signature is not a byte array');
  if (!payload.equals(Buffer.from(messageHash))) {
    return {
      isValid: false,
      message: 'Signature verification failed',
      error: 'Payload in signature does not match hash of provided message',
    };
  }
  const sigStructure = encodeCborArray([
    'Signature1',
    protectedHeader,
    Buffer.alloc(0),
    payload,
  ]);
  const isValid = ed25519Verify(publicKey, sigStructure, signature);
  return {
    isValid,
    message: isValid ? 'Signature is valid' : 'Signature verification failed',
  };
}

export function verifyEd25519(
  publicKeyHex: unknown,
  signatureHex: unknown,
  messageHash: Uint8Array,
): SignatureVerificationResult {
  const isValid = ed25519Verify(
    hexBytes(publicKeyHex, 'publicKey', 32),
    messageHash,
    hexBytes(signatureHex, 'signature', 64),
  );
  return {
    isValid,
    message: isValid ? 'Signature is valid' : 'Signature verification failed',
  };
}

/* ------------------------------------------------------------------------- */
/* The signed message                                                         */
/* ------------------------------------------------------------------------- */

/** Resolves a remote JSON-LD context URL to its parsed document. */
export type ContextLoader = (url: string) => Promise<unknown>;

/** Remote contexts one document may pull in, nested ones included. */
const MAX_REMOTE_CONTEXTS = 8;

/**
 * A jsonld document loader over `load`, bounded in count. jsonld wraps a
 * loader's error in its own, so the first failure is kept to report as is.
 */
function contextLoader(load: ContextLoader | undefined) {
  const state: { failure?: SignatureInputError } = {};
  let loaded = 0;
  const fail = (message: string): never => {
    state.failure ??= new SignatureInputError(message);
    throw state.failure;
  };
  const documentLoader = async (url: string) => {
    const shown = url.slice(0, 200);
    if (load === undefined) fail(`remote JSON-LD context not loaded: ${shown}`);
    if (++loaded > MAX_REMOTE_CONTEXTS) {
      fail('Metadata references too many remote JSON-LD contexts');
    }
    let document: unknown;
    try {
      document = await load!(url);
    } catch {
      fail(`remote JSON-LD context could not be loaded: ${shown}`);
    }
    return { contextUrl: undefined, documentUrl: url, document };
  };
  return { state, documentLoader };
}

/**
 * blake2b-256 of the URDNA2015 canonical form of `{ @context, body }`.
 * Remote contexts resolve through `load`; without one they are refused.
 */
export async function hashedBody(
  document: Record<string, unknown>,
  load?: ContextLoader,
): Promise<Uint8Array> {
  if (!document.body || typeof document.body !== 'object') {
    throw new SignatureInputError('Metadata does not contain body field');
  }
  const loader = contextLoader(load);
  let canonical: string;
  try {
    canonical = await jsonld.canonize(
      {
        '@context': document['@context'],
        body: document.body,
      } as jsonld.JsonLdDocument,
      {
        algorithm: 'URDNA2015',
        format: 'application/n-quads',
        documentLoader:
          loader.documentLoader as unknown as jsonld.Options.DocLoader['documentLoader'],
      },
    );
  } catch (error) {
    throw loader.state.failure ?? error;
  }
  return blake.blake2b(new TextEncoder().encode(canonical), undefined, 32);
}

export type AuthorWitnessInput = {
  author?: {
    name?: unknown;
    witness?: {
      witnessAlgorithm?: unknown;
      publicKey?: unknown;
      signature?: unknown;
    };
  };
};

/**
 * Verifies one author's witness against an already fetched document, with
 * remote contexts resolved through `load`. Every failure is a result, never a
 * throw, as the UI expects.
 */
export async function verifyAuthorWitness(
  input: AuthorWitnessInput,
  document: Record<string, unknown>,
  load?: ContextLoader,
): Promise<SignatureVerificationResult> {
  try {
    const witness = input.author?.witness;
    const algorithm = witness?.witnessAlgorithm;
    if (typeof algorithm !== 'string' || algorithm === '') {
      throw new SignatureInputError('Algorithm is missing in witness');
    }
    if (!witness?.publicKey || !witness.signature) {
      throw new SignatureInputError(
        'Missing publicKey or signature in witness',
      );
    }
    const hash = await hashedBody(document, load);
    switch (algorithm.toLowerCase()) {
      case 'ed25519':
        return verifyEd25519(witness.publicKey, witness.signature, hash);
      case 'cip-0008':
        return verifyCip8(witness.publicKey, witness.signature, hash);
      default:
        throw new SignatureInputError(
          `Unsupported witness algorithm: ${algorithm.slice(0, 40)}`,
        );
    }
  } catch (error) {
    return {
      isValid: false,
      error:
        error instanceof SignatureInputError
          ? error.message
          : 'Signature could not be verified',
    };
  }
}
