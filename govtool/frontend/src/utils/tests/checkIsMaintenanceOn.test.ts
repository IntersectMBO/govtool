import { describe, it, expect, vi, afterEach } from "vitest";
import axios from "axios";
import { checkIsMaintenanceOn } from "..";

vi.stubGlobal("location", {
  ...window.location,
  reload: vi.fn(),
});

const axiosGetSpy = vi.spyOn(axios, "get");

describe("checkIsMaintenanceOn function", () => {
  afterEach(() => {
    axiosGetSpy.mockClear();
    vi.resetAllMocks();
  });

  it("does nothing in development mode", async () => {
    vi.stubEnv("VITE_IS_DEV", "true");
    await checkIsMaintenanceOn();
    expect(axiosGetSpy).not.toHaveBeenCalled();
    expect(window.location.reload).not.toHaveBeenCalled();
  });

  it("reloads the page if maintenance mode is active", async () => {
    vi.stubEnv("VITE_IS_DEV", "");
    axiosGetSpy.mockResolvedValue({ data: true });
    await checkIsMaintenanceOn();
    expect(window.location.reload).toHaveBeenCalled();
  });

  it("does not reload the page if maintenance mode is not active", async () => {
    vi.stubEnv("VITE_IS_DEV", "");
    axiosGetSpy.mockResolvedValue({ data: false });
    await checkIsMaintenanceOn();
    expect(window.location.reload).not.toHaveBeenCalled();
  });

  it("throws an error if the request fails", async () => {
    vi.stubEnv("VITE_IS_DEV", "");
    axiosGetSpy.mockRejectedValue(new Error("Network Error"));
    await expect(checkIsMaintenanceOn()).rejects.toThrow(
      "Action canceled due to maintenance mode.",
    );
  });
});
