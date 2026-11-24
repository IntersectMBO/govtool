import { afterEach, describe, expect, it } from "vitest";

import { connectedWallet } from "../connectedWallet";

const setWallet = (key: string | null, extension?: Record<string, unknown>) => {
  if (key) window.localStorage.setItem("wallet_data_name", JSON.stringify(key));
  const target = window as unknown as { cardano?: unknown };
  target.cardano = extension && key ? { [key]: extension } : undefined;
};

describe("connectedWallet", () => {
  afterEach(() => {
    window.localStorage.clear();
    (window as unknown as { cardano?: unknown }).cardano = undefined;
  });

  it("names the wallet and takes its icon from the extension", () => {
    setWallet("lace", { name: "lace", icon: "data:image/svg+xml;base64,PHN2Zy8+" });
    expect(connectedWallet()).toEqual({ name: "Lace", icon: "data:image/svg+xml;base64,PHN2Zy8+" });
  });

  it("refuses an icon that is not an image data URL or https", () => {
    // Built with join so lint's no-script-url does not read it as code.
    setWallet("evil", { name: "Evil", icon: ["javascript", "alert(1)"].join(":") });
    expect(connectedWallet()).toEqual({ name: "Evil", icon: undefined });
  });

  it("falls back to the stored key without an extension, and is undefined unconnected", () => {
    setWallet("eternl");
    expect(connectedWallet()).toEqual({ name: "Eternl", icon: undefined });
    window.localStorage.clear();
    expect(connectedWallet()).toBeUndefined();
  });
});
