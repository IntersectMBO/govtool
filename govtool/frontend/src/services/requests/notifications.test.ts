import { beforeEach, describe, expect, it, vi } from "vitest";

import { getNotificationsConfig, getNotificationsPage } from "./notifications";

const api = vi.hoisted(() => ({ get: vi.fn(), post: vi.fn(), put: vi.fn() }));
vi.mock("../GovernanceActionsAPI", () => ({ GovernanceActionsAPI: api }));

describe("getNotificationsConfig", () => {
  beforeEach(() => {
    api.get.mockReset();
  });

  it("is undefined while no notifications service runs", async () => {
    api.get.mockResolvedValue({ status: 404, data: "Not Found" });
    await expect(getNotificationsConfig()).resolves.toBeUndefined();
  });

  it("is undefined for a network failure or an unknown version", async () => {
    api.get.mockRejectedValueOnce(new Error("offline"));
    await expect(getNotificationsConfig()).resolves.toBeUndefined();
    api.get.mockResolvedValueOnce({ status: 200, data: { version: 2 } });
    await expect(getNotificationsConfig()).resolves.toBeUndefined();
  });

  it("accepts a later version that declares itself compatible with 1, and rejects a malformed one", async () => {
    api.get.mockResolvedValueOnce({
      status: 200,
      data: { version: 2, compatible: [1], channels: { inApp: true, email: false }, events: [] },
    });
    await expect(getNotificationsConfig()).resolves.toMatchObject({ version: 2 });
    api.get.mockResolvedValueOnce({ status: 200, data: { version: 1 } });
    await expect(getNotificationsConfig()).resolves.toBeUndefined();
  });

  it("returns a version 1 config", async () => {
    const config = {
      version: 1,
      channels: { inApp: true, email: true },
      events: [],
      frequencies: [],
    };
    api.get.mockResolvedValue({ status: 200, data: config });
    await expect(getNotificationsConfig()).resolves.toEqual(config);
  });
});

describe("getNotificationsPage", () => {
  it("sends the token and the paging parameters", async () => {
    api.get.mockResolvedValue({ data: { items: [], total: 0, unread: 0, counts: {} } });
    await getNotificationsPage("tok", { page: 2, pageSize: 7, filter: "unread", sort: "oldest" });
    expect(api.get).toHaveBeenCalledWith("/notifications", {
      headers: { Authorization: "Bearer tok" },
      params: { page: 2, pageSize: 7, filter: "unread", sort: "oldest" },
    });
  });
});
