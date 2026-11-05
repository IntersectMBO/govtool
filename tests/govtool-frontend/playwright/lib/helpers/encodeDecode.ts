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

/** The CIP-129 gov_action1… id that names an action in GovTool's URLs. */
export const getGovActionCIP129Id = (
  txHash: string,
  index: number | string = 0
) =>
  encodeCIP129Identifier({
    txID: txHash,
    index: Number(index).toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });

/** The URL id for "txHash#index"; anything else is returned as given. */
export const toGovActionUrlId = (id: string) => {
  const [txHash, index] = id.split("#");
  return /^[0-9a-fA-F]{64}$/.test(txHash)
    ? getGovActionCIP129Id(txHash, index || 0)
    : id;
};

/** The transaction hash and index a gov_action1… id names. */
export const parseGovActionCIP129Id = (id: string) => {
  const bytes = Buffer.from(bech32.fromWords(bech32.decode(id).words));
  return {
    txHash: bytes.subarray(0, 32).toString("hex"),
    index: parseInt(bytes.subarray(32).toString("hex") || "0", 16),
  };
};
