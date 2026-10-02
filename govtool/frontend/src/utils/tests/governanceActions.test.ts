import { describe, expect, it } from "vitest";

import { encodeCIP129Identifier } from "../cip129identifier";
import {
  encodeGovernanceActionCommitteeColdId,
  getGovernanceActionCIP129Id,
  getGovernanceActionProposalStatus,
  getGovernanceActionVoteEnd,
  toGovernanceActionId,
} from "../governanceActions";

const TX_HASH =
  "a2a6e4ff9f1a2d1c1d0b6b6a4e0c9a2b8c0d9e8f7a6b5c4d3e2f1a0b9c8d7e6f";

const status = (overrides: Partial<Record<string, number | null>> = {}) => ({
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
  ...overrides,
});

describe("getGovernanceActionProposalStatus", () => {
  it("is Live with no terminal epoch", () => {
    expect(getGovernanceActionProposalStatus(status())).toBe("Live");
  });

  it("prefers Enacted over Ratified", () => {
    expect(
      getGovernanceActionProposalStatus(
        status({ ratified_epoch: 10, enacted_epoch: 11 }),
      ),
    ).toBe("Enacted");
  });

  it("prefers Expired over Not Ratified", () => {
    expect(
      getGovernanceActionProposalStatus(
        status({ expired_epoch: 10, dropped_epoch: 11 }),
      ),
    ).toBe("Expired");
    expect(getGovernanceActionProposalStatus(status({ dropped_epoch: 11 }))).toBe(
      "Not Ratified",
    );
  });
});

describe("toGovernanceActionId", () => {
  it("passes CIP-105 ids and free text through", () => {
    expect(toGovernanceActionId(`${TX_HASH}#0`)).toBe(`${TX_HASH}#0`);
    expect(toGovernanceActionId("treasury")).toBe("treasury");
  });

  it("decodes a CIP-129 id to txHash#index with a decimal index", () => {
    const id = encodeCIP129Identifier({
      txID: TX_HASH,
      index: (10).toString(16).padStart(2, "0"),
      bech32Prefix: "gov_action",
    });
    expect(toGovernanceActionId(id)).toBe(`${TX_HASH}#10`);
  });

  it("returns a malformed gov_action id unchanged", () => {
    expect(toGovernanceActionId("gov_action1notbech32")).toBe(
      "gov_action1notbech32",
    );
  });

  it("round-trips getGovernanceActionCIP129Id", () => {
    expect(
      toGovernanceActionId(getGovernanceActionCIP129Id({ tx_hash: TX_HASH, index: 3 })),
    ).toBe(`${TX_HASH}#3`);
  });
});

describe("encodeGovernanceActionCommitteeColdId", () => {
  const keyHash = "a".repeat(56);

  it("encodes key and script credentials with their CIP-129 headers", () => {
    const keyId = encodeGovernanceActionCommitteeColdId(keyHash, false);
    const scriptId = encodeGovernanceActionCommitteeColdId(keyHash, true);
    expect(keyId.startsWith("cc_cold1")).toBe(true);
    expect(scriptId.startsWith("cc_cold1")).toBe(true);
    expect(keyId).not.toBe(scriptId);
  });

  it("returns an empty string for a hash that is not 28 bytes", () => {
    expect(encodeGovernanceActionCommitteeColdId("abc")).toBe("");
  });
});

describe("getGovernanceActionVoteEnd", () => {
  const times = {
    ratified_time: "2026-09-01T00:00:00.000Z",
    enacted_time: "2026-09-02T00:00:00.000Z",
    dropped_time: "2026-09-03T00:00:00.000Z",
    expired_time: "2026-09-04T00:00:00.000Z",
  };

  it("is null while the action is live", () => {
    expect(
      getGovernanceActionVoteEnd({ status: status(), status_times: times }),
    ).toBeNull();
  });

  it("ends an enacted action at its ratification", () => {
    expect(
      getGovernanceActionVoteEnd({
        status: status({ ratified_epoch: 10, enacted_epoch: 11 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Enacted", time: times.ratified_time, epoch: 10 });
  });

  it("ends an expired or dropped action at that epoch", () => {
    expect(
      getGovernanceActionVoteEnd({
        status: status({ expired_epoch: 20, dropped_epoch: 21 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Expired", time: times.expired_time, epoch: 20 });
    expect(
      getGovernanceActionVoteEnd({
        status: status({ dropped_epoch: 21 }),
        status_times: times,
      }),
    ).toEqual({ outcome: "Not Ratified", time: times.dropped_time, epoch: 21 });
  });
});
