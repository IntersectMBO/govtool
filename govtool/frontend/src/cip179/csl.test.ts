import {
  BigNum,
  Int,
  TransactionMetadatum,
} from "@emurgo/cardano-serialization-lib-asmjs";
import {
  decodePayload,
  encodeMetadata,
  Role,
  type Metadatum,
  type SurveyResponse,
} from "cip-179";

import {
  buildAuxiliaryData,
  fromTransactionMetadatum,
  metadatumCodec,
  toTransactionMetadatum,
} from "./csl";

describe("CIP-179 CSL adapter", () => {
  it("round-trips every transaction metadatum shape", () => {
    const value: Metadatum = new Map<Metadatum, Metadatum>([
      [0n, -2n],
      [1n, "text"],
      [2n, new Uint8Array([0xaa, 0xbb])],
      [3n, [1n, "nested"]],
    ]);
    const encoded = toTransactionMetadatum(value);
    const decoded = TransactionMetadatum.from_bytes(encoded.to_bytes());
    expect(decoded.as_map().len()).toBe(4);
    const zero = TransactionMetadatum.new_int(Int.new_i32(0));
    expect(decoded.as_map().get(zero)?.as_int().to_json()).toBe('"-2"');
  });

  it("encodes metadatum integers as native CBOR integers", () => {
    expect(Buffer.from(metadatumCodec.metadatumToCbor(1n)).toString("hex")).toBe(
      "01",
    );
    expect(Buffer.from(metadatumCodec.metadatumToCbor(-1n)).toString("hex")).toBe(
      "20",
    );
    expect(metadatumCodec.cborToMetadatum(new Uint8Array([0x19, 0x01, 0x00]))).toBe(
      256n,
    );
  });

  it("places a CIP-179 response under metadata label 17", () => {
    const response: SurveyResponse = {
      specVersion: 5,
      surveyRef: { txId: new Uint8Array(32), index: 0 },
      role: Role.DRep,
      credential: { type: "key", keyHash: new Uint8Array(28) },
      answers: {
        type: "public",
        answers: [{ type: "numeric", questionIndex: 0, value: 4n }],
      },
    };
    const encoded = encodeMetadata({ type: "responses", responses: [response] });
    if (!(encoded instanceof Map)) throw new Error("Expected metadata map");
    const auxiliaryData = buildAuxiliaryData(encoded);
    const label17 = auxiliaryData.metadata()?.get(BigNum.from_str("17"));
    expect(label17).toBeDefined();
    expect(decodePayload(fromTransactionMetadatum(label17!))).toEqual({
      type: "responses",
      responses: [response],
    });
  });
});
