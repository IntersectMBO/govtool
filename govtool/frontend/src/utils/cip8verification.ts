// This is slightly refactored version of the CIP-8 verification code from govtool-outcomes-pillar:
// https://github.com/IntersectMBO/govtool-outcomes-pillar/commit/c268532f5ea0edde9aca338f66f1b9f265450db3

import * as CardanoASMJS from "@emurgo/cardano-serialization-lib-asmjs";
import * as cbor from "cbor-web";
import { Buffer } from "buffer";

const isHex = (text: string) => /^[0-9a-fA-F]+$/.test(text);

const trimString = (s: string) =>
  s.replace(/(^\s*)|(\s*$)/gi, "").replace(/\n /, "\n");

export async function verifyCIP8Signature(
  signature: string,
  message: string,
  vkey: string,
): Promise<boolean> {
  try {
    if (!isHex(signature) || !isHex(vkey) || !isHex(message)) {
      throw new Error("Signature, vkey, and message must be valid hex strings");
    }

    const publicKey = createPublicKey(vkey);
    const signatureStructure = createSignatureStructure(signature);
    const sigStructureCborHex = Buffer.from(
      cbor.encode(signatureStructure.signature),
    ).toString("hex");

    const payloadHex = signatureStructure.payloadBuffer.toString("hex");

    if (message !== payloadHex) {
      console.error(
        "Signature verification failed - Payload in signature does not match hash of provided message",
      );
      return false;
    }

    const signatureHex = signatureStructure.signatureBuffer.toString("hex");

    return publicKey.verify(
      Buffer.from(sigStructureCborHex, "hex"),
      CardanoASMJS.Ed25519Signature.from_hex(signatureHex),
    );
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    throw new Error(`Signature verification failed: ${errorMessage}`);
  }
}

const createPublicKey = (coseKeyCborHex: string) => {
  let publicKey;
  let coseKeyStructure;

  coseKeyCborHex = trimString(coseKeyCborHex.toLowerCase());
  try {
    coseKeyStructure = cbor.decode(Buffer.from(coseKeyCborHex, "hex"));
  } catch {
    // If not CBOR, try as raw vkey
    try {
      publicKey = getPublicKeyFromVkey(coseKeyCborHex);
    } catch {
      throw new Error(
        "Input is neither a valid COSE_Key CBOR nor a valid vkey hex string",
      );
    }
  }

  if (!publicKey && coseKeyStructure) {
    // Validate COSE Key structure
    if (!(coseKeyStructure instanceof Map) || coseKeyStructure.size < 4) {
      throw new Error(
        "COSE_Key is not valid. It must be a map with at least 4 entries: kty,alg,crv,x.",
      );
    }
    if (coseKeyStructure.get(1) !== 1) {
      throw new Error('COSE_Key map label "1" (kty) is not "1" (OKP)');
    }
    if (coseKeyStructure.get(3) !== -8) {
      throw new Error('COSE_Key map label "3" (alg) is not "-8" (EdDSA)');
    }
    if (coseKeyStructure.get(-1) !== 6) {
      throw new Error('COSE_Key map label "-1" (crv) is not "6" (Ed25519)');
    }
    if (!coseKeyStructure.has(-2)) {
      throw new Error('COSE_Key map label "-2" (public key) is missing');
    }
    const pubKeyBuffer = coseKeyStructure.get(-2);
    if (!Buffer.isBuffer(pubKeyBuffer)) {
      throw new Error("PublicKey entry in the COSE_Key is not a bytearray");
    }
    const pubKey = pubKeyBuffer.toString("hex");
    publicKey = CardanoASMJS.PublicKey.from_bytes(Buffer.from(pubKey, "hex"));
  }
  if (!publicKey) {
    throw new Error("Public key could not be derived from COSE_Key");
  }
  return publicKey;
};

const createSignatureStructure = (signature: string) => {
  const coseSign1CborHex = trimString(signature.toLowerCase());
  // Decode COSE_Sign1
  let coseSign1Structure;
  try {
    coseSign1Structure = cbor.decode(Buffer.from(coseSign1CborHex, "hex"));
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    throw new Error(
      `Can't cbor decode the given COSE_Sign1 signature: (${errorMessage})`,
    );
  }

  // Validate COSE_Sign1 structure
  if (!Array.isArray(coseSign1Structure) || coseSign1Structure.length !== 4) {
    throw new Error(
      "COSE_Sign1 is not a valid signature. It must be an array with 4 entries.",
    );
  }

  // Extract content from coseSign1Structure
  const [
    protectedHeaderBuffer,
    unprotectedHeader,
    payloadBuffer,
    signatureBuffer,
  ] = coseSign1Structure;

  if (!Buffer.isBuffer(signatureBuffer)) {
    throw new Error("Signature is not a bytearray");
  }

  // 1) Validate and decode protected header
  if (!Buffer.isBuffer(protectedHeaderBuffer)) {
    throw new Error("Protected header is not a bytearray (serialized) cbor");
  }

  let protectedHeader;
  try {
    protectedHeader = ensureMap(cbor.decode(protectedHeaderBuffer));
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    throw new Error(`Can't cbor decode the protected header (${errorMessage})`);
  }

  if (!protectedHeader.has(1)) {
    throw new Error('Protected header map label "1" is missing');
  }
  if (protectedHeader.get(1) !== -8) {
    throw new Error('Protected header map label "1" (alg) is not "-8" (EdDSA)');
  }
  if (!protectedHeader.has("address")) {
    throw new Error('Protected header map label "address" is missing');
  }

  // 2) Process unprotectedHeader as in executable
  ensureMap(unprotectedHeader);

  return {
    signature: [
      "Signature1",
      protectedHeaderBuffer,
      Buffer.from(""),
      payloadBuffer,
    ],
    payloadBuffer,
    signatureBuffer,
  };
};

function getPublicKeyFromVkey(vkey: string): CardanoASMJS.PublicKey {
  try {
    return CardanoASMJS.PublicKey.from_bytes(Buffer.from(vkey, "hex"));
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error);
    throw new Error(`Invalid vkey format: ${errorMessage}`);
  }
}

function ensureMap<T = unknown>(value: unknown): Map<unknown, T> {
  if (value instanceof Map) return value;
  if (typeof value === "object" && value !== null)
    return new Map(Object.entries(value));
  throw new Error("Value is not a map");
}
