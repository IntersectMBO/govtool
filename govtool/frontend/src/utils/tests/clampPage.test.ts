import { describe, expect, it } from "vitest";

import { clampPage } from "../clampPage";

describe("clampPage", () => {
  it("keeps a page that still exists", () => {
    expect(clampPage(2, 25, 10)).toBe(2);
  });

  it("moves back to the last page when the total shrinks", () => {
    expect(clampPage(3, 15, 10)).toBe(2);
  });

  it("stays on page 1 when nothing is left", () => {
    expect(clampPage(4, 0, 10)).toBe(1);
  });
});
