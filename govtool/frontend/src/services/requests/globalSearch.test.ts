import { beforeEach, describe, expect, it, vi } from "vitest";

import { lookupStakeAddress } from "./globalSearch";

const get = vi.hoisted(() => vi.fn());
vi.mock("../GovernanceActionsAPI", () => ({ GovernanceActionsAPI: { get } }));

const STAKE = "stake1u8p3nxkyq98vpzretevjpef8d76j2z9dacfq46l6vk94hdql5m037";
const notFound = Object.assign(new Error("404"), { response: { status: 404 } });

describe("lookupStakeAddress", () => {
  beforeEach(() => {
    get.mockReset();
  });

  it("reads an address the backend has never seen as not registered", async () => {
    get.mockImplementation(async (path: string) => {
      if (path.startsWith("/account/")) throw notFound;
      if (path.includes("current-delegation")) return { data: null };
      return { data: 0 };
    });
    await expect(lookupStakeAddress(STAKE)).resolves.toEqual({
      stakeAddress: STAKE,
      isRegistered: false,
      delegation: null,
      votingPower: 0,
    });
  });

  it("leaves a part out when the backend fails it for another reason", async () => {
    get.mockImplementation(async (path: string) => {
      if (path.startsWith("/account/")) throw new Error("down");
      if (path.includes("current-delegation")) return { data: null };
      return { data: 5 };
    });
    expect((await lookupStakeAddress(STAKE)).isRegistered).toBeUndefined();
  });

  it("fails when every part fails", async () => {
    get.mockRejectedValue(new Error("down"));
    await expect(lookupStakeAddress(STAKE)).rejects.toThrow("down");
  });
});
