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

    URL.new = vi.fn().mockReturnValueOnce({});
    AnchorDataHash.from_hex = vi.fn().mockReturnValueOnce({});
    Anchor.new = vi.fn().mockReturnValueOnce({});

    const spyForAnchor = vi.spyOn(Anchor, "new").mockReturnValue(new Anchor());
    const anchor = generateAnchor(url, hash);

    expect(URL.new).toHaveBeenCalledWith(url);
    expect(AnchorDataHash.from_hex).toHaveBeenCalledWith(hash);
    expect(spyForAnchor).toHaveBeenCalledWith({}, {});
    expect(anchor).toBeInstanceOf(Anchor);

    spyForAnchor.mockRestore();
  });
});
