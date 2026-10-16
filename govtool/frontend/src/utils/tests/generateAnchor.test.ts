import { vi } from "vitest";
import {
  Anchor,
  AnchorDataHash,
  URL,
} from "@emurgo/cardano-serialization-lib-asmjs";
import { generateAnchor } from "..";

describe("generateAnchor function", () => {
  it("generates an anchor with the provided URL and hash", () => {
    const url = "https://example.com";
    const hash = "aabbccddeeff";
    const expectedAnchor = new Anchor();

    URL.new = vi.fn().mockReturnValueOnce(url);
    AnchorDataHash.from_hex = vi.fn().mockReturnValueOnce(hash);
    const spyForAnchor = vi.spyOn(Anchor, "new").mockReturnValue(expectedAnchor);

    const anchor = generateAnchor(url, hash);

    expect(URL.new).toHaveBeenCalledWith(url);
    expect(AnchorDataHash.from_hex).toHaveBeenCalledWith(hash);
    expect(spyForAnchor).toHaveBeenCalledWith(url, hash);
    expect(anchor).toBe(expectedAnchor);

spyForAnchor.mockRestore();
  });
});
