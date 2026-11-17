import { beforeEach, describe, expect, it, vi } from "vitest";

import {
  lookupStakeAddress,
  searchDReps,
  searchGovernanceActions,
} from "./globalSearch";

const get = vi.hoisted(() => vi.fn());
const getHandleDetails = vi.hoisted(() => vi.fn());
vi.mock("../GovernanceActionsAPI", () => ({ GovernanceActionsAPI: { get } }));
vi.mock("@/services/AdaHandle", () => ({
  adaHandleService: {
    getHandleDetails,
    getAdaHandleCIP105DRepId: async (handle: string) =>
      (await getHandleDetails(handle))?.drep?.cip_105 ?? "",
  },
}));

describe("searchGovernanceActions", () => {
  beforeEach(() => {
    get.mockReset();
    get.mockResolvedValue({ data: { elements: [], total: 0 } });
  });

  it("sends a pasted CIP-129 id as it is, so its index is kept", async () => {
    const id = "gov_action1w2w64uhelz0cg2np7m37hal905tdd7jpzm3fcyc3g7qvkwgfppgqqfsggt5";
    await searchGovernanceActions(id, 0, 5);
    expect(get).toHaveBeenCalledWith("/proposal/list", {
      params: { page: 0, pageSize: 5, search: id },
    });
  });
});

describe("searchDReps", () => {
  const fetchSpy = vi.fn();
  beforeEach(() => {
    get.mockReset();
    getHandleDetails.mockReset();
    fetchSpy.mockReset();
    vi.stubGlobal("fetch", fetchSpy);
    get.mockResolvedValue({
      data: {
        elements: [{ view: "drep1abc", isScriptBased: false, imageUrl: "https://dreps.example/me.png" }],
        total: 1,
      },
    });
  });

  it("fetches no DRep images and asks handle.me nothing for a keyword", async () => {
    const { elements } = await searchDReps("treasury", 0, 5);
    expect(elements[0].view).toBe("drep1abc");
    expect(fetchSpy).not.toHaveBeenCalled();
    expect(getHandleDetails).not.toHaveBeenCalled();
  });

  it("resolves a $handle to its DRep", async () => {
    getHandleDetails.mockResolvedValue({ drep: { cip_105: "drep1resolved" } });
    await searchDReps("$someone", 0, 5);
    expect(get).toHaveBeenCalledWith("/drep/list", {
      params: { page: 0, pageSize: 5, search: "drep1resolved", listed: true },
    });
  });
});

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
