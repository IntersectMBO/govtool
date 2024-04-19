import { vi } from "vitest";
import { checkIsMaintenanceOn } from "..";
import axios from "axios";

vi.mock("axios");
vi.stubEnv("VITE_IS_DEV", "");

describe("checkIsMaintenanceOn function", () => {
  it("does not reload the page when maintenance mode is off", async () => {
    const location: Location = window.location;
    delete window.location;
    window.location = {
      ...location,
      reload: vi.fn(),
    };
    axios.get.mockResolvedValueOnce({ data: false });

    const somethingSpy = vi.spyOn(window.location, "reload").mockReturnValue();

    await checkIsMaintenanceOn();

    expect(somethingSpy).not.toHaveBeenCalled();
  });

  it("does not reload the page when maintenance mode is on", async () => {
    const location: Location = window.location;
    delete window.location;
    window.location = {
      ...location,
      reload: vi.fn(),
    };
    axios.get.mockResolvedValueOnce({ data: true });

    const somethingSpy = vi.spyOn(window.location, "reload").mockReturnValue();

    await checkIsMaintenanceOn();

    expect(somethingSpy).toHaveBeenCalledTimes(1);
  });
});
