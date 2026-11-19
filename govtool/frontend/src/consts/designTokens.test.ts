import { describe, expect, it } from "vitest";

import { ds, NOTICE_TONES, PILL_TONES } from "./designTokens";

// WCAG 2.1 relative luminance and contrast ratio.
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
// The nav's active pill: #FF640A at 8% (0x14) over white.
const NAV_TINT = "#FFF3EC";

describe("text colours meet WCAG AA (4.5:1)", () => {
  it("the header notices' text and links on their tints", () => {
    Object.values(NOTICE_TONES).forEach((tone) => {
      expect(contrast(tone.link, tone.linkBg)).toBeGreaterThanOrEqual(4.5);
      expect(contrast(ds.gray["900"], tone.bg)).toBeGreaterThanOrEqual(4.5);
    });
  });

  it("the active nav item on its orange tint", () => {
    expect(contrast(ds.orange["750"], NAV_TINT)).toBeGreaterThanOrEqual(4.5);
  });

  it("orange links and white text on the orange button", () => {
    expect(contrast(ds.orange["700"], "#FFFFFF")).toBeGreaterThanOrEqual(4.5);
  });

  it("muted text on the page backgrounds", () => {
    ["#FFFFFF", "#FCFCFA", ds.gray["25"], ds.gray["50"], ds.gray["100"]].forEach((bg) =>
      expect(contrast(ds.gray["500"], bg)).toBeGreaterThanOrEqual(4.5),
    );
  });

  it("every pill tone", () => {
    Object.values(PILL_TONES).forEach(({ bg, text }) =>
      expect(contrast(text, bg)).toBeGreaterThanOrEqual(4.5),
    );
  });
});
