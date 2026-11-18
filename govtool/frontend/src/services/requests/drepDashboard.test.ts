import { beforeEach, describe, expect, it, vi } from "vitest";

const { get, axiosGet } = vi.hoisted(() => ({ get: vi.fn(), axiosGet: vi.fn() }));

vi.mock("../GovernanceActionsAPI", () => ({ GovernanceActionsAPI: { get } }));
vi.mock("axios", () => ({ default: { get: axiosGet } }));
vi.mock("@/config/env", () => ({ env: { VITE_PDF_API_URL: "https://forum.test/" } }));

// eslint-disable-next-line import/first
import { getAllGovernanceActions, getDRepDiscussions } from "./drepDashboard";

describe("getAllGovernanceActions", () => {
  beforeEach(() => {
    get.mockReset();
  });

  it("walks pages of 100 until a short page", async () => {
    const page = (n: number, size: number) =>
      Array.from({ length: size }, (_, i) => ({ id: `${n}-${i}` }));
    get
      .mockResolvedValueOnce({ data: page(1, 100) })
      .mockResolvedValueOnce({ data: page(2, 100) })
      .mockResolvedValueOnce({ data: page(3, 7) });

    const actions = await getAllGovernanceActions();

    expect(actions).toHaveLength(207);
    expect(get).toHaveBeenCalledTimes(3);
    expect(get).toHaveBeenLastCalledWith("/governance-actions", {
      params: { search: "", filters: "", sort: "newestFirst", page: 3, limit: 100 },
    });
  });

  it("stops at the page cap", async () => {
    get.mockResolvedValue({
      data: Array.from({ length: 100 }, (_, i) => ({ id: String(i) })),
    });
    await getAllGovernanceActions();
    expect(get).toHaveBeenCalledTimes(30);
  });
});

describe("getDRepDiscussions", () => {
  const thread = (id: number, userId: string, name: string) => ({
    id,
    attributes: {
      user_id: userId,
      user_govtool_username: name,
      prop_likes: 4,
      prop_comments_number: 9,
      createdAt: "2026-11-01T00:00:00Z",
      content: {
        attributes: {
          prop_name: `Thread ${id}`,
          prop_abstract: "Abstract",
          gov_action_type: { attributes: { gov_action_type_name: "Treasury" } },
        },
      },
    },
  });

  beforeEach(() => {
    axiosGet.mockReset();
  });

  it("finds the DRep's forum account through its comments", async () => {
    axiosGet.mockImplementation((url: string) => {
      if (url.includes("/api/comments?") && url.includes("drep_id")) {
        return Promise.resolve({
          data: {
            data: [
              { id: 1, attributes: { proposal_id: 7, user_id: "42", createdAt: "2026-11-02T00:00:00Z", user_govtool_username: "me" } },
              { id: 2, attributes: { proposal_id: 9, user_id: "42", createdAt: "2026-11-01T00:00:00Z", user_govtool_username: "me" } },
            ],
          },
        });
      }
      if (url.includes("filters[user_id][$eq]=42")) {
        return Promise.resolve({ data: { data: [thread(3, "42", "me")] } });
      }
      if (url.includes("filters[prop_id][$in]")) {
        return Promise.resolve({ data: { data: [thread(7, "5", "other"), thread(9, "6", "third")] } });
      }
      return Promise.reject(new Error(`unexpected ${url}`));
    });

    const threads = await getDRepDiscussions("ab".repeat(28));

    expect(axiosGet.mock.calls[0][0]).toBe(
      `https://forum.test/api/comments?filters[drep_id][$eq]=${"ab".repeat(28)}` +
        "&sort[createdAt]=desc&pagination[page]=1&pagination[pageSize]=100",
    );
    expect(threads.map(({ id, isOwn, repliedByYou }) => ({ id, isOwn, repliedByYou }))).toEqual([
      { id: 7, isOwn: false, repliedByYou: true },
      { id: 3, isOwn: true, repliedByYou: false },
      { id: 9, isOwn: false, repliedByYou: true },
    ]);
    expect(threads[0]).toMatchObject({
      title: "Thread 7",
      topic: "Treasury",
      author: "other",
      replies: 9,
      likes: 4,
      lastReply: { author: "me", at: "2026-11-02T00:00:00Z", isOwn: true },
    });
  });

  it("asks only for the active revision of each thread replied to", async () => {
    axiosGet.mockImplementation((url: string) =>
      Promise.resolve({
        data: {
          data: url.includes("/api/comments?")
            ? [{ id: 1, attributes: { proposal_id: 7, user_id: "x", createdAt: "2026-11-02T00:00:00Z" } }]
            : [],
        },
      }));
    await getDRepDiscussions("ab".repeat(28));
    const replied = axiosGet.mock.calls
      .map(([url]) => String(url))
      .find((url) => url.includes("filters[prop_id][$in]"));
    expect(replied).toContain("filters[prop_rev_active][$eq]=true");
  });

  it("is empty without a comment, and never sends a malformed id", async () => {
    axiosGet.mockResolvedValue({ data: { data: [] } });
    expect(await getDRepDiscussions("ab".repeat(28))).toEqual([]);
    await expect(getDRepDiscussions("not hex&x=1")).resolves.toEqual([]);
    expect(axiosGet).toHaveBeenCalledTimes(1);
  });
});
