import { encodePayload, Role, type SurveyDefinition } from "cip-179";

import {
  decodeDefinition,
  getRenderabilityProblem,
  MAX_RENDERED_SURVEY_ITEMS,
  parseSurveyLink,
} from "./core";
import { buildAnswer } from "./Cip179Survey";
import { metadatumCodec } from "./csl";
import dbSyncFixture from "./fixtures/dbSyncMetadata.json";

const definition: SurveyDefinition = {
  specVersion: 5,
  owner: { type: "key", keyHash: new Uint8Array(28) },
  title: "Priorities",
  description: "Choose one",
  eligibleRoles: [Role.DRep],
  endEpoch: 500,
  submissionMode: { type: "public" },
  questions: [
    {
      type: "singleChoice",
      prompt: "Priority?",
      options: { type: "options", labels: ["A", "B"] },
      required: true,
    },
  ],
};

describe("CIP-179 integration helpers", () => {
  // Independently encoded fixture following db-sync's Map.singleton key md.
  // Pin: cardano-db-sync eb8e862de8f4076523eb4146905bab7eac1d5e1e,
  // Cardano/DbSync/Era/Universal/Insert/Tx.hs, insertTxMetadata.
  it.each([
    ["inner payload", dbSyncFixture.payloadOnlyCborHex, 0, "Audit"],
    ["db-sync row", dbSyncFixture.dbSyncRowCborHex, 0, "Audit"],
    ["nonzero batch index", dbSyncFixture.batchedDbSyncRowCborHex, 1, "Second"],
  ])("decodes %s", (_name, payloadCborHex, index, title) => {
    const link = { txId: dbSyncFixture.txId, index: Number(index) };
    expect(
      decodeDefinition(
        {
          ...link,
          surveyIndex: link.index,
          metadataLabel: 17,
          payloadCborHex: String(payloadCborHex),
        },
        link,
        501,
      ).title,
    ).toBe(title);
  });

  it.each([
    ["wrong metadata label", `a112${dbSyncFixture.payloadOnlyCborHex}`],
    ["string metadata label", `a1623137${dbSyncFixture.payloadOnlyCborHex}`],
    ["empty map", "a0"],
    ["invalid label payload", "a11100"],
    ["truncated CBOR", dbSyncFixture.dbSyncRowCborHex.slice(0, -2)],
    ["cancellation payload", `a111820281825820${"11".repeat(32)}00`],
  ])("rejects %s", (_name, payloadCborHex) => {
    expect(() =>
      decodeDefinition(
        {
          txId: dbSyncFixture.txId,
          surveyIndex: 0,
          metadataLabel: 17,
          payloadCborHex,
        },
        { txId: dbSyncFixture.txId, index: 0 },
        501,
      ),
    ).toThrow();
  });

  it("parses only complete revision 5 links", () => {
    const txId = "ab".repeat(32);
    expect(
      parseSurveyLink({
        body: {
          cip179: {
            specVersion: 5,
            kind: "survey-link",
            surveyTxId: txId.toUpperCase(),
            surveyIndex: 0,
          },
        },
      }),
    ).toEqual({ txId, index: 0 });
    expect(
      parseSurveyLink({
        body: { cip179: { kind: "survey-link", surveyTxId: txId, surveyIndex: 0 } },
      }),
    ).toBeNull();
  });

  it("decodes exact label payload CBOR and checks action expiry", () => {
    const payload = encodePayload({ type: "definitions", definitions: [definition] });
    const payloadCborHex = Buffer.from(
      metadatumCodec.metadatumToCbor(payload),
    ).toString("hex");
    const link = { txId: "ab".repeat(32), index: 0 };
    expect(
      decodeDefinition(
        { txId: "ab".repeat(32), surveyIndex: 0, metadataLabel: 17, payloadCborHex },
        link,
        501,
      ),
    ).toEqual(definition);
    expect(() =>
      decodeDefinition(
        { txId: "ab".repeat(32), surveyIndex: 0, metadataLabel: 17, payloadCborHex },
        link,
        502,
      ),
    ).toThrow("end epoch");
  });

  it("rejects a definition response for a different survey reference", () => {
    const payloadCborHex = Buffer.from(
      metadatumCodec.metadatumToCbor(
        encodePayload({ type: "definitions", definitions: [definition] }),
      ),
    ).toString("hex");
    expect(() =>
      decodeDefinition(
        { txId: "cd".repeat(32), surveyIndex: 0, metadataLabel: 17, payloadCborHex },
        { txId: "ab".repeat(32), index: 0 },
        501,
      ),
    ).toThrow("requested reference");
  });

  it("bounds the number of rendered options", () => {
    const withOptionCount = (count: number): SurveyDefinition => ({
      ...definition,
      questions: [
        {
          type: "singleChoice",
          prompt: "Choose",
          options: {
            type: "options",
            labels: Array.from({ length: count }, (_, index) => String(index)),
          },
        },
      ],
    });
    expect(
      getRenderabilityProblem(withOptionCount(MAX_RENDERED_SURVEY_ITEMS)),
    ).toBeNull();
    expect(
      getRenderabilityProblem(withOptionCount(MAX_RENDERED_SURVEY_ITEMS + 1)),
    ).toContain("more than 100 options");
    expect(
      getRenderabilityProblem({
        ...definition,
        questions: Array.from(
          { length: MAX_RENDERED_SURVEY_ITEMS + 1 },
          () => definition.questions[0],
        ),
      }),
    ).toContain("more than 100 questions");
    expect(
      getRenderabilityProblem({
        ...definition,
        questions: [
          {
            type: "rating",
            prompt: "Rate",
            options: { type: "options", labels: ["A", "B"] },
            scale: { type: "count", count: MAX_RENDERED_SURVEY_ITEMS + 1 },
            requireAll: false,
          },
        ],
      }),
    ).toContain("more than 100 rating levels");
  });

  it("treats an incomplete numeric rating as invalid instead of throwing", () => {
    expect(
      buildAnswer(
        {
          type: "rating",
          prompt: "Impact",
          options: { type: "options", labels: ["A", "B"] },
          scale: { type: "numeric", constraints: { min: 1n, max: 5n } },
          requireAll: false,
        },
        0,
        ["-", null],
        true,
      ),
    ).toBeNull();
  });

  it("rejects an empty rating answer instead of encoding an empty pair list", () => {
    expect(
      buildAnswer(
        {
          type: "rating",
          prompt: "Impact",
          options: { type: "options", labels: ["A", "B"] },
          scale: { type: "numeric", constraints: { min: 1n, max: 5n } },
          requireAll: false,
        },
        0,
        [null, null],
        true,
      ),
    ).toBeNull();
  });

  it("rejects fractional points allocations before response validation", () => {
    expect(
      buildAnswer(
        {
          type: "pointsAllocation",
          prompt: "Allocate",
          options: { type: "options", labels: ["A", "B"] },
          budget: 10,
        },
        0,
        [1.5, 8.5],
        true,
      ),
    ).toBeNull();
  });
});
