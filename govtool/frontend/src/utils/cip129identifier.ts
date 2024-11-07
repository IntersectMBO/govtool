import { bech32 } from "bech32";
import { Buffer } from "buffer";

/**
 * Encodes a CIP129 identifier based on the provided transaction ID, index, and bech32 prefix.
 * @param txID - The transaction ID.
 * @param index - The index.
 * @param bech32Prefix - The bech32 prefix.
 * @returns The generated CIP129 identifier.
 */
export const encodeCIP129Identifier = (
  txID: string,
  index: string,
  bech32Prefix: string,
) => {
  const govActionBytes = Buffer.from(txID + index, "hex");
  const words = bech32.toWords(govActionBytes);
  return bech32.encode(bech32Prefix, words);
};

/**
 * Decodes a CIP129 identifier.
 * @param cip129Identifier - The CIP129 identifier to decode.
 * @returns An object containing the decoded transaction ID, index, and prefix.
 */
export const decodeCIP129Identifier = (cip129Identifier: string) => {
  const { prefix, words } = bech32.decode(cip129Identifier);
  const buffer = Buffer.from(bech32.fromWords(words));
  const txID = buffer.subarray(0, 32).toString("hex");
  const index = buffer.subarray(32).toString("hex");
  return { txID, index, prefix };
};
