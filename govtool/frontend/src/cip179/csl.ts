import {
  AuxiliaryData,
  BigNum,
  GeneralTransactionMetadata,
  Int,
  MetadataList,
  MetadataMap,
  TransactionMetadatum,
  TransactionMetadatumKind,
} from "@emurgo/cardano-serialization-lib-asmjs";
import type { Metadatum, MetadatumMap } from "cip-179";

export const toTransactionMetadatum = (
  value: Metadatum,
): TransactionMetadatum => {
  if (typeof value === "bigint") {
    const int =
      value >= 0n
        ? Int.new(BigNum.from_str(value.toString()))
        : Int.new_negative(BigNum.from_str((-value).toString()));
    return TransactionMetadatum.new_int(int);
  }
  if (typeof value === "string") return TransactionMetadatum.new_text(value);
  if (value instanceof Uint8Array) {
    return TransactionMetadatum.new_bytes(value);
  }
  if (value instanceof Map) {
    const map = MetadataMap.new();
    value.forEach((item, key) =>
      map.insert(toTransactionMetadatum(key), toTransactionMetadatum(item)),
    );
    return TransactionMetadatum.new_map(map);
  }
  const list = MetadataList.new();
  value.forEach((item) => list.add(toTransactionMetadatum(item)));
  return TransactionMetadatum.new_list(list);
};

export const fromTransactionMetadatum = (
  value: TransactionMetadatum,
): Metadatum => {
  switch (value.kind()) {
    case TransactionMetadatumKind.Int:
      return BigInt(value.as_int().to_str());
    case TransactionMetadatumKind.Text:
      return value.as_text();
    case TransactionMetadatumKind.Bytes:
      return new Uint8Array(value.as_bytes());
    case TransactionMetadatumKind.MetadataList: {
      const list = value.as_list();
      return Array.from({ length: list.len() }, (_, index) =>
        fromTransactionMetadatum(list.get(index)),
      );
    }
    case TransactionMetadatumKind.MetadataMap: {
      const map = value.as_map();
      const keys = map.keys();
      return new Map(
        Array.from({ length: keys.len() }, (_, index) => {
          const key = keys.get(index);
          return [
            fromTransactionMetadatum(key),
            fromTransactionMetadatum(map.get(key)),
          ];
        }),
      );
    }
    default:
      throw new Error("Unsupported transaction metadatum kind");
  }
};

export const metadatumCodec = {
  metadatumToCbor: (value: Metadatum): Uint8Array =>
    toTransactionMetadatum(value).to_bytes(),
  cborToMetadatum: (bytes: Uint8Array): Metadatum =>
    fromTransactionMetadatum(TransactionMetadatum.from_bytes(bytes)),
};

export const buildAuxiliaryData = (
  transactionMetadata: MetadatumMap,
): AuxiliaryData => {
  const metadata = GeneralTransactionMetadata.new();
  transactionMetadata.forEach((value, label) => {
    if (typeof label !== "bigint" || label < 0n) {
      throw new Error("Transaction metadata labels must be non-negative integers");
    }
    metadata.insert(
      BigNum.from_str(label.toString()),
      toTransactionMetadatum(value),
    );
  });
  const auxiliaryData = AuxiliaryData.new();
  auxiliaryData.set_metadata(metadata);
  return auxiliaryData;
};
