import { describe, it, expect, vi } from "vitest";
import { getCip95Wallets } from "../getCip95Wallets";

describe("getCip95Wallets", () => {
  it("should return an array of CIP-95 wallets", () => {
    const cardanoWindowObject: typeof window.cardano = {
      wallet1: {
        icon: "icon1",
        name: "wallet1",
        supportedExtensions: [{ cip: 95 }],
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
      wallet2: {
        icon: "icon2",
        name: "wallet2",
        supportedExtensions: [{ cip: 95 }],
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
      duplicateNameWallet: {
        icon: "icon3",
        name: "wallet1",
        supportedExtensions: [{ cip: 95 }],
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
      incompatibleWallet: {
        icon: "icon3",
        name: "incompatibleWallet",
        supportedExtensions: [{ cip: 94 }],
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
      emptyArrayWallet: {
        icon: "icon4",
        name: "emptyArrayWallet",
        supportedExtensions: [],
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
      // For testing purposes
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-expect-error
      noExtensionsWallet: {
        icon: "icon4",
        name: "noExtensionsWallet",
        apiVersion: "1.0.0",
        enable: vi.fn(),
        isEnabled: vi.fn(),
      },
    };
    const result = getCip95Wallets(cardanoWindowObject);
    const expected = [
      {
        icon: "icon1",
        label: "wallet1",
        name: "wallet1",
        cip95Available: true,
      },
      {
        icon: "icon2",
        label: "wallet2",
        name: "wallet2",
        cip95Available: true,
      },
    ];
    expect(result).toStrictEqual(expected);
  });
});
