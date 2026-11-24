import { describe, expect, it } from "vitest";

import { classifyWalletError, networkLabel } from "../walletConnect";

describe("classifyWalletError", () => {
  it("reads the network mismatch message", () => {
    expect(
      classifyWalletError(
        "Error: You are trying to connect with a wallet connected to testnet. Please adjust your wallet settings to connect to mainnet or select a different wallet.",
      ),
    ).toEqual({ kind: "network", walletNetwork: "testnet", requiredNetwork: "mainnet" });
  });

  it("knows a declined request", () => {
    ["user declined", "User rejected the request", "Refused", "user canceled connection"].forEach((text) =>
      expect(classifyWalletError(text).kind).toBe("declined"));
  });

  it("does not read maintenance as a declined request", () => {
    expect(classifyWalletError("Action canceled due to maintenance mode.").kind).toBe("generic");
  });

  it("knows a wallet without the needed extensions", () => {
    expect(classifyWalletError("Your wallet does not support CIP-30 extensions.").kind).toBe("unsupported");
    expect(classifyWalletError("Your wallet did not enable the needed CIP-95 functions during connection.").kind)
      .toBe("unsupported");
  });

  it("falls back to a generic failure", () => {
    expect(classifyWalletError("No addresses found")).toEqual({ kind: "generic" });
    expect(classifyWalletError(undefined)).toEqual({ kind: "generic" });
  });
});

describe("networkLabel", () => {
  it("names each network", () => {
    expect(networkLabel("mainnet")).toBe("Cardano Mainnet");
    expect(networkLabel("preprod")).toBe("Cardano Preprod");
    expect(networkLabel("preview")).toBe("Cardano Preview");
    expect(networkLabel("sanchonet")).toBe("SanchoNet");
    expect(networkLabel("testnet")).toBe("Cardano Testnet");
  });
});
