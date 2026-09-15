import { bech32 } from "bech32";
import { Decoder, Encoder } from "cbor-x";

export const cborxEncoder = new Encoder({
  mapsAsObjects: false,
  useRecords: false,
});
export const cborxDecoder = new Decoder({ mapsAsObjects: false });

export const encodeCIP129Identifier = ({
  txID,
  index,
  bech32Prefix,
}: {
  txID: string;
  index?: string;
  bech32Prefix: string;
}) => {
  const govActionBytes = Buffer.from(index ? txID + index : txID, "hex");
  const words = bech32.toWords(govActionBytes);
  return bech32.encode(bech32Prefix, words);
};
