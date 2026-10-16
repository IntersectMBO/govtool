import { describe, it, expect, vi, afterEach } from "vitest";
import axios from "axios";
import { checkIsMaintenanceOn } from "../checkIsMaintenanceOn";
import { env } from "@/config/env";

vi.stubGlobal("location", {
  ...window.location,
  reload: vi.fn(),
});

const axiosGetSpy = vi.spyOn(axios, "get");

/** Answers the down flag and maintenance.json the way nginx would. */
const serve = ({
  isDown = false,
  notice,
}: {
  isDown?: boolean;
  notice?: object;
}) =>
  axiosGetSpy.mockImplementation(async (url: string) =>
    (url.endsWith("/is-maintenance-mode-on")
      ? { status: 200, data: isDown, headers: {} }
      : notice
      ? { status: 200, data: notice, headers: {} }
      : { status: 404, data: "Not Found", headers: {} }),
  );

describe("checkIsMaintenanceOn function", () => {
  afterEach(() => {
    axiosGetSpy.mockClear();
    vi.resetAllMocks();
  });

  it("does nothing in development mode", async () => {
    env.VITE_IS_DEV = "true";
    await checkIsMaintenanceOn({ blockDuringNotice: true });
    expect(axiosGetSpy).not.toHaveBeenCalled();
    expect(window.location.reload).not.toHaveBeenCalled();
  });

  it("asks the page's own origin, port included", async () => {
    env.VITE_IS_DEV = "";
    serve({});
    await checkIsMaintenanceOn();
    expect(axiosGetSpy).toHaveBeenCalledWith(
      `${window.location.origin}/is-maintenance-mode-on`,
    );
  });

  it("reloads the page if the site is down for maintenance", async () => {
    env.VITE_IS_DEV = "";
    serve({ isDown: true });
    await checkIsMaintenanceOn();
    expect(window.location.reload).toHaveBeenCalled();
  });

  it("does not reload the page if maintenance mode is not active", async () => {
    env.VITE_IS_DEV = "";
    serve({});
    await checkIsMaintenanceOn();
    expect(window.location.reload).not.toHaveBeenCalled();
  });

  it("does not reload for a dev server's index.html", async () => {
    env.VITE_IS_DEV = "";
    axiosGetSpy.mockResolvedValue({ status: 200, data: "<html></html>" });
    await checkIsMaintenanceOn();
    expect(window.location.reload).not.toHaveBeenCalled();
  });

  it("throws an error if the request fails", async () => {
    env.VITE_IS_DEV = "";
    axiosGetSpy.mockRejectedValue(new Error("Network Error"));
    await expect(checkIsMaintenanceOn()).rejects.toThrow(
      "Action canceled due to maintenance mode.",
    );
  });

  it("refuses transactions while the maintenance banner is up", async () => {
    env.VITE_IS_DEV = "";
    serve({ notice: { endsAt: "2026-10-16T12:30:00Z" } });
    await expect(
      checkIsMaintenanceOn({ blockDuringNotice: true }),
    ).rejects.toThrow(/transactions are paused/);
  });

  it("lets other wallet actions through while the banner is up", async () => {
    env.VITE_IS_DEV = "";
    serve({ notice: { endsAt: "2026-10-16T12:30:00Z" } });
    await expect(checkIsMaintenanceOn()).resolves.toBeUndefined();
  });

  it("lets transactions through when there is no notice", async () => {
    env.VITE_IS_DEV = "";
    serve({});
    await expect(
      checkIsMaintenanceOn({ blockDuringNotice: true }),
    ).resolves.toBeUndefined();
  });
});
