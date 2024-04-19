import { vi } from "vitest";
import { generateAnchor } from "..";
import {
  Anchor,
  AnchorDataHash,
  URL,
} from "@emurgo/cardano-serialization-lib-asmjs";

describe("generateAnchor function", () => {
  it("generates an anchor with the provided URL and hash", () => {
    const url = "https://example.com";
    const hash = "aabbccddeeff";

    URL.new = vi.fn().mockReturnValueOnce({});
    AnchorDataHash.from_hex = vi.fn().mockReturnValueOnce({});
    Anchor.new = vi.fn().mockReturnValueOnce({});

    const anchor = generateAnchor(url, hash);

    expect(URL.new).toHaveBeenCalledWith(url);

    expect(AnchorDataHash.from_hex).toHaveBeenCalledWith(hash);

    const spyFoAnchor = vi.spyOn(Anchor, "new").mockReturnValue();

    expect(spyFoAnchor).toHaveBeenCalledWith([{}, {}]);

    expect(anchor).toBeInstanceOf(Anchor);
  });
});
