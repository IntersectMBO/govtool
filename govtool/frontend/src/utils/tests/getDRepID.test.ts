import { vi } from "vitest";
import { getPubDRepID } from "../getDRepID";
import { CardanoApiWallet } from "@/models";

const mockGetPubDRepKey = vi.fn();

const mockWalletApi = {
  cip95: {
    getPubDRepKey: mockGetPubDRepKey,
  },
} as unknown as CardanoApiWallet;

describe("getPubDRepID function", () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  it("returns the dRepKey and dRepID when walletApi returns a valid response", async () => {
    const dRepKey = "dRepKey123";
    mockGetPubDRepKey.mockResolvedValueOnce(dRepKey);
    const result = await getPubDRepID(mockWalletApi);
    expect(result).toEqual({
      dRepKey,
      dRepID: expect.any(String),
    });
    expect(mockGetPubDRepKey).toHaveBeenCalled();
  });

  it("returns undefined values for dRepKey and dRepID when walletApi throws an error", async () => {
    const failedToGetPubDRepKeyError = new Error("Failed to get PubDRepKey");
    mockGetPubDRepKey.mockRejectedValueOnce(failedToGetPubDRepKeyError);

    const consoleErrorSpy = vi.spyOn(console, "error");
    const result = await getPubDRepID(mockWalletApi);

    expect(result).toEqual({
      dRepKey: undefined,
      dRepID: undefined,
    });

    expect(consoleErrorSpy).toHaveBeenCalledWith(failedToGetPubDRepKeyError);
  });
});
