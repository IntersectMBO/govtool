import { vi } from "vitest";
import { openInNewTab } from "..";

describe("openInNewTab function", () => {
  it("opens a new tab with the provided URL", () => {
    const originalOpen = window.open;
    const mockOpen = vi.fn();
    window.open = mockOpen;

    const url = "https://example.com";
    openInNewTab(url);

    expect(mockOpen).toHaveBeenCalledWith(url, "_blank", "noopener,noreferrer");

    window.open = originalOpen;
  });

  it("sets opener to null if new window is opened", () => {
    const originalOpen = window.open;
    const mockNewWindow = {
      opener: "someOpener",
    };
    const mockOpen = vi.fn().mockReturnValue(mockNewWindow);
    window.open = mockOpen;

    const url = "https://example.com";
    openInNewTab(url);

    expect(mockNewWindow.opener).toBeNull();

    window.open = originalOpen;
  });
});
