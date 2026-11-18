import { afterEach, describe, expect, it, vi } from "vitest";

import { AVATAR_TONES, downloadCsv, shortId, VOTE_PILL_TONES } from "./parts";

const luminance = (hex: string) => {
  const [r, g, b] = [1, 3, 5].map((at) => {
    const channel = parseInt(hex.slice(at, at + 2), 16) / 255;
    return channel <= 0.03928 ? channel / 12.92 : ((channel + 0.055) / 1.055) ** 2.4;
  });
  return 0.2126 * r + 0.7152 * g + 0.0722 * b;
};
const contrast = (a: string, b: string) => {
  const [light, dark] = [luminance(a), luminance(b)].sort((x, y) => y - x);
  return (light + 0.05) / (dark + 0.05);
};

describe("dashboard colours meet WCAG AA (4.5:1)", () => {
  it("vote chips", () => {
    Object.values(VOTE_PILL_TONES).forEach(({ text, bg }) =>
      expect(contrast(text, bg)).toBeGreaterThanOrEqual(4.5));
  });

  it("initials on their avatar colour", () => {
    AVATAR_TONES.forEach(({ fg, bg }) => expect(contrast(fg, bg)).toBeGreaterThanOrEqual(4.5));
  });
});

describe("shortId", () => {
  it("keeps only the head when no tail is asked for", () => {
    expect(shortId("stake1u9abcdefghijklmnop", 8, 0)).toBe("stake1u9…");
  });
});

describe("downloadCsv", () => {
  afterEach(() => {
    vi.useRealTimers();
  });

  it("revokes the file URL only after the download has started", () => {
    vi.useFakeTimers();
    URL.createObjectURL = vi.fn(() => "blob:x");
    URL.revokeObjectURL = vi.fn();
    const click = vi.spyOn(HTMLAnchorElement.prototype, "click").mockImplementation(() => {});
    downloadCsv("a.csv", "x");
    expect(click).toHaveBeenCalled();
    expect(URL.revokeObjectURL).not.toHaveBeenCalled();
    vi.runAllTimers();
    expect(URL.revokeObjectURL).toHaveBeenCalledWith("blob:x");
    click.mockRestore();
  });
});
