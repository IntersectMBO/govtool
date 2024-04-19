import { vi } from "vitest";
import { formHexToBech32, getPubDRepID } from "../getDRepID";

const dRepIdHex = "99f2c9a961ff53099796643a514a0640379b706ad310bc751c2997c9";
const dRepIdBech32 = "drep1n8evn2tplafsn9ukvsa9zjsxgqmekur26vgtcagu9xtujzv2yv8";

describe("formHexToBech32 function", () => {
  it("returns correct dRep bech32 format", () => {
    const bech32Format = formHexToBech32(dRepIdHex);

    expect(bech32Format).toBe(dRepIdBech32);
  });

  it("expected undefined when no argument", () => {
    const bech32Format = formHexToBech32();

    expect(bech32Format).toBe(undefined);
  });
});

const mockGetPubDRepKey = vi.fn();

const mockWalletApi = {
  cip95: {
    getPubDRepKey: mockGetPubDRepKey,
  },
};

describe("getPubDRepID function", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("returns the dRepKey, dRepID, and dRepIDBech32 when walletApi returns a valid response", async () => {
    const dRepKey = "dRepKey123";
    mockGetPubDRepKey.mockResolvedValueOnce(dRepKey);

    const result = await getPubDRepID(mockWalletApi);

    expect(result).toEqual({
      dRepKey,
      dRepID: expect.any(String),
      dRepIDBech32: expect.any(String),
    });

    expect(mockGetPubDRepKey).toHaveBeenCalled();
  });

  it("returns undefined values for dRepKey, dRepID, and dRepIDBech32 when walletApi throws an error", async () => {
    mockGetPubDRepKey.mockRejectedValueOnce(
      new Error("Failed to get PubDRepKey"),
    );

    const result = await getPubDRepID(mockWalletApi);

    expect(result).toEqual({
      dRepKey: undefined,
      dRepID: undefined,
      dRepIDBech32: undefined,
    });

    expect(mockGetPubDRepKey).toHaveBeenCalled();
  });
});
