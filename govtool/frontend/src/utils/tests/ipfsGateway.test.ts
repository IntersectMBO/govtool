import { describe, it, expect, vi, beforeEach } from "vitest";

import { getIpfsGateway, ipfsGatewayUrl } from "../ipfsGateway";

const mockEnv = vi.hoisted((): { VITE_IPFS_GATEWAY?: string } => ({}));
vi.mock("@/config/env", () => ({ env: mockEnv }));

describe("ipfsGateway", () => {
  beforeEach(() => {
    delete mockEnv.VITE_IPFS_GATEWAY;
  });

  it("defaults to ipfs.io when VITE_IPFS_GATEWAY is unset", () => {
    expect(getIpfsGateway()).toBe("https://ipfs.io/ipfs");
    expect(ipfsGatewayUrl("bafkreiabc")).toBe("https://ipfs.io/ipfs/bafkreiabc");
  });

  it("uses VITE_IPFS_GATEWAY, tolerating a trailing slash", () => {
    mockEnv.VITE_IPFS_GATEWAY = "http://localhost:3000/ipfs/";
    expect(ipfsGatewayUrl("bafkreiabc/doc.json")).toBe(
      "http://localhost:3000/ipfs/bafkreiabc/doc.json",
    );
  });
});
