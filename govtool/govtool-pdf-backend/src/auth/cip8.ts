import { blake, CoseKey, CoseSign1, Ed25519Key, KeyType } from 'libcardano';

const HEX_RE = /^(?:[0-9a-fA-F]{2})+$/;
/** Generous caps; a CIP-30 signData result is a few hundred bytes. */
const MAX_SIGNATURE_HEX = 16384;
const MAX_KEY_HEX = 2048;

export interface Cip8Input {
  /** Hex COSE_Sign1, as CIP-30 signData returns it. */
  signature: unknown;
  /** Hex COSE_Key. */
  key: unknown;
  /** The challenge message the payload must equal (Δ15). */
  message: string;
  /** Lowercase hex blake2b-224 of the expected public key. */
  expectedKeyHash: string;
}

const isBytes = (v: unknown): v is Uint8Array => v instanceof Uint8Array;

/**
 * SPEC §7.3, through libcardano only. Resolves true or false; never throws,
 * so malformed input can never become a 500.
 *
 * Not checked: the protected `address` header. The Playwright wallet puts the
 * base address there when signing for a reward address or key hash; the key
 * hash (step 4) and the payload (step 5) already bind key and challenge.
 */
export async function verifyCip8(input: Cip8Input): Promise<boolean> {
  try {
    const { signature, key } = input;
    if (typeof signature !== 'string' || typeof key !== 'string') return false;
    if (signature.length > MAX_SIGNATURE_HEX || key.length > MAX_KEY_HEX) return false;
    if (!HEX_RE.test(signature) || !HEX_RE.test(key)) return false;

    // 1. Decode.
    const sign1 = CoseSign1.fromBytes(Buffer.from(signature, 'hex'));
    const coseKey = CoseKey.fromBytes(Buffer.from(key, 'hex'));
    if (!isBytes(sign1.headers.protectedHeaders) || !isBytes(sign1.rawSignature)) return false;
    // A detached (nil) payload fails.
    if (!isBytes(sign1.payload)) return false;

    // 2. OKP key with alg EdDSA and a 32-byte public key.
    if (coseKey.keyType !== KeyType.OKP) return false;
    const pub: unknown = coseKey.pubKeyBytes;
    if (!isBytes(pub) || pub.length !== 32) return false;
    const edKey = Ed25519Key.fromCoseKey(coseKey); // throws unless alg = -8

    // 3. Ed25519 over ["Signature1", protected, h'', payload].
    if (!(await sign1.verify(edKey))) return false;

    // 4. Key hash.
    if (Buffer.from(edKey.pkh).toString('hex') !== input.expectedKeyHash.toLowerCase()) return false;

    // 5. Payload binding.
    const unprotected: unknown = sign1.headers.unprotectedHeaders.getData();
    const hashed =
      unprotected instanceof Map ? (unprotected as Map<unknown, unknown>).get('hashed') : undefined;
    const msg = Buffer.from(input.message, 'utf8');
    const expected = hashed === true ? blake.hash28(msg) : msg;
    return Buffer.compare(Buffer.from(sign1.payload), expected) === 0;
  } catch {
    return false;
  }
}
