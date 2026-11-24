import { describe, expect, it, vi } from "vitest";

const { get } = vi.hoisted(() => ({ get: vi.fn() }));
vi.mock("../GovernanceActionsAPI", () => ({ GovernanceActionsAPI: { get } }));

// eslint-disable-next-line import/first
import { getStakeKeyVotingPower } from "./walletConnect";

describe("getStakeKeyVotingPower", () => {
  it("reads lovelace, and refuses an answer that is not a number", async () => {
    get.mockResolvedValueOnce({ data: 5_000_000 });
    await expect(getStakeKeyVotingPower("e1ab")).resolves.toBe(5_000_000);
    get.mockResolvedValueOnce({ data: "not a number" });
    await expect(getStakeKeyVotingPower("e1ab")).rejects.toThrow();
  });
});
