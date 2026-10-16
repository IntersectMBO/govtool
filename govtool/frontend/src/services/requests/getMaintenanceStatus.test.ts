import axios from "axios";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { getMaintenanceStatus } from "./getMaintenanceStatus";

const NOW = Date.parse("2026-10-16T12:00:00Z");

const axiosGet = vi.spyOn(axios, "get");

const respond = (
  status: number,
  data: unknown,
  headers: Record<string, string> = {},
) => axiosGet.mockResolvedValue({ status, data, headers });

describe("getMaintenanceStatus", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    vi.setSystemTime(NOW);
  });

  afterEach(() => {
    vi.useRealTimers();
    axiosGet.mockReset();
  });

  it("reads the notice from the page's own origin", async () => {
    respond(200, {
      startsAt: "2026-10-16T11:30:00Z",
      endsAt: "2026-10-16T12:30:00Z",
    });

    await expect(getMaintenanceStatus()).resolves.toEqual({
      startsAt: Date.parse("2026-10-16T11:30:00Z"),
      endsAt: Date.parse("2026-10-16T12:30:00Z"),
    });
    expect(axiosGet).toHaveBeenCalledWith(
      `${window.location.origin}/maintenance.json`,
      expect.anything(),
    );
  });

  it("is null when no notice is published", async () => {
    respond(404, "<html>Not Found</html>");

    await expect(getMaintenanceStatus()).resolves.toBeNull();
  });

  it("is null for a dev server's index.html fallback", async () => {
    respond(200, "<!doctype html><html></html>");

    await expect(getMaintenanceStatus()).resolves.toBeNull();
  });

  it("keeps a notice without usable times", async () => {
    respond(200, { endsAt: "soon" });

    await expect(getMaintenanceStatus()).resolves.toEqual({
      startsAt: undefined,
      endsAt: undefined,
    });
  });

  it("moves the times onto a browser clock that is off", async () => {
    // The server says it is 10 minutes later than this browser thinks.
    respond(
      200,
      { endsAt: "2026-10-16T12:30:00Z" },
      { date: new Date(NOW + 600_000).toUTCString() },
    );

    await expect(getMaintenanceStatus()).resolves.toMatchObject({
      endsAt: Date.parse("2026-10-16T12:20:00Z"),
    });
  });

  it("ignores a few seconds of clock difference", async () => {
    respond(
      200,
      { endsAt: "2026-10-16T12:30:00Z" },
      { date: new Date(NOW + 2_000).toUTCString() },
    );

    await expect(getMaintenanceStatus()).resolves.toMatchObject({
      endsAt: Date.parse("2026-10-16T12:30:00Z"),
    });
  });
});
