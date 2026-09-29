import { describe, expect, it } from "vitest";

import { encodeCIP129Identifier } from "../cip129identifier";
import {
  encodeOutcomeCommitteeColdId,
  formatOutcomeVoteValue,
  getOutcomeCIP129Id,
  getOutcomeProposalStatus,
  getOutcomeVoteEnd,
  lovelaceToRoundedUpAda,
  toOutcomeGovActionId,
} from "../outcomes";

const TX_HASH =
  "a2a6e4ff9f1a2d1c1d0b6b6a4e0c9a2b8c0d9e8f7a6b5c4d3e2f1a0b9c8d7e6f";

const status = (overrides: Partial<Record<string, number | null>> = {}) => ({
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  ...overrides,
});

describe("getOutcomeProposalStatus", () => {
  it("is Live with no terminal epoch", () => {
    expect(getOutcomeProposalStatus(status())).toBe("Live");
  });

  it("prefers Enacted over Ratified", () => {
    expect(
      getOutcomeProposalStatus(
        status({ ratified_epoch: 10, enacted_epoch: 11 }),
      ),
    ).toBe("Enacted");
  });

  it("prefers Expired over Not Ratified", () => {
    expect(
      getOutcomeProposalStatus(
        status({ expired_epoch: 10, dropped_epoch: 11 }),
      ),
    ).toBe("Expired");
    expect(getOutcomeProposalStatus(status({ dropped_epoch: 11 }))).toBe(
      "Not Ratified",
    );
  });
});

describe("toOutcomeGovActionId", () => {
  it("passes CIP-105 ids and free text through", () => {
    expect(toOutcomeGovActionId(`${TX_HASH}#0`)).toBe(`${TX_HASH}#0`);
    expect(toOutcomeGovActionId("treasury")).toBe("treasury");
  });

  it("decodes a CIP-129 id to txHash#index with a decimal index", () => {
    const id = encodeCIP129Identifier({
      txID: TX_HASH,
      index: (10).toString(16).padStart(2, "0"),
      bech32Prefix: "gov_action",
    });
    expect(toOutcomeGovActionId(id)).toBe(`${TX_HASH}#10`);
  });

  it("returns a malformed gov_action id unchanged", () => {
    expect(toOutcomeGovActionId("gov_action1notbech32")).toBe(
      "gov_action1notbech32",
    );
  });

  it("round-trips getOutcomeCIP129Id", () => {
    expect(
      toOutcomeGovActionId(getOutcomeCIP129Id({ tx_hash: TX_HASH, index: 3 })),
    ).toBe(`${TX_HASH}#3`);
  });
});

describe("encodeOutcomeCommitteeColdId", () => {
  const keyHash = "a".repeat(56);

  it("encodes key and script credentials with their CIP-129 headers", () => {
    const keyId = encodeOutcomeCommitteeColdId(keyHash, false);
    const scriptId = encodeOutcomeCommitteeColdId(keyHash, true);
    expect(keyId.startsWith("cc_cold1")).toBe(true);
    expect(scriptId.startsWith("cc_cold1")).toBe(true);
    expect(keyId).not.toBe(scriptId);
  });

  it("returns an empty string for a hash that is not 28 bytes", () => {
    expect(encodeOutcomeCommitteeColdId("abc")).toBe("");
  });
});

describe("vote value formatting", () => {
  it("shows the committee count as is", () => {
    expect(formatOutcomeVoteValue(5, true)).toBe(5);
  });

  it("shows stake as suffixed ada", () => {
    expect(formatOutcomeVoteValue(2_500_000_000_000, false)).toBe("₳ 2.50M");
  });

  it("rounds lovelace up to whole ada", () => {
    expect(lovelaceToRoundedUpAda(1_000_001)).toBe(2);
    expect(lovelaceToRoundedUpAda(undefined)).toBe(0);
  });
});

describe("getOutcomeVoteEnd", () => {
  const times = {
    ratified_time: "2026-09-01T00:00:00.000Z",
    enacted_time: "2026-09-02T00:00:00.000Z",
    dropped_time: "2026-09-03T00:00:00.000Z",
    expired_time: "2026-09-04T00:00:00.000Z",
  };

  it("is null while the action is live", () => {
    expect(
      getOutcomeVoteEnd({ status: status(), status_times: times }),
    ).toBeNull();
  });

  it("ends an enacted action at its ratification", () => {
    expect(
      getOutcomeVoteEnd({
        status: status({ ratified_epoch: 10, enacted_epoch: 11 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Enacted", time: times.ratified_time, epoch: 10 });
  });

  it("ends an expired or dropped action at that epoch", () => {
    expect(
      getOutcomeVoteEnd({
        status: status({ expired_epoch: 20, dropped_epoch: 21 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Expired", time: times.expired_time, epoch: 20 });
    expect(
      getOutcomeVoteEnd({
        status: status({ dropped_epoch: 21 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Not Ratified", time: times.dropped_time, epoch: 21 });
  });
});
