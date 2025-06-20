/**
 * @vitest-environment node
 *
 * We are switching to the "node" environment because jsdom does not provide a full WebCrypto API,
 * specifically window.crypto.subtle, which is required by @noble/ed25519 for signature
 * verification. Node.js (v16.8+) includes native crypto.subtle support, making it suitable for
 * cryptographic tests. Current browsers support window.crypto.subtle natively, so this issue only
 * affects the test environment. */

import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { validateSignature } from "../validateSignature";

describe("validateSignature", () => {
  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    vi.restoreAllMocks();
    vi.clearAllMocks();
  });

  it("returns false if signature is missing", async () => {
    const result = await validateSignature({
      ...validInput,
      signature: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if publicKey is missing", async () => {
    const result = await validateSignature({
      ...validInput,
      publicKey: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if algorithm is missing", async () => {
    const result = await validateSignature({
      ...validInput,
      algorithm: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if message is missing", async () => {
    const result = await validateSignature({
      ...validInput,
      jsonContent: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false for unsupported algorithm", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const result = await validateSignature({
      ...validInput,
      algorithm: "rsa",
    });
    expect(result).toBe(false);
    expect(console.error).toHaveBeenCalledWith("Unsupported algorithm:", "rsa");
  });

  describe("Ed25519 algorithm", () => {
    it("accepts 'Ed25519' (case-insensitive)", async () => {
      const result = await validateSignature({
        ...validInput,
        algorithm: "Ed25519",
      });

      expect(result).toBe(true);
    });

    it("accepts 'ed25519' (case-insensitive)", async () => {
      const result = await validateSignature({
        ...validInput,
        algorithm: "ed25519",
      });
      expect(result).toBe(true);
    });
  });
});

const validInput = {
  jsonContent: {
    "@context": {
      "@language": "en",
      CIP100:
        "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
      CIP108:
        "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#",
      authors: {
        "@container": "@set",
        "@context": {
          name: "http://xmlns.com/foaf/0.1/name",
          witness: {
            "@context": {
              publicKey: "CIP100:publicKey",
              signature: "CIP100:signature",
              witnessAlgorithm: "CIP100:witnessAlgorithm",
            },
            "@id": "CIP100:witness",
          },
        },
        "@id": "CIP100:authors",
      },
      body: {
        "@context": {
          abstract: "CIP108:abstract",
          motivation: "CIP108:motivation",
          rationale: "CIP108:rationale",
          references: {
            "@container": "@set",
            "@context": {
              GovernanceMetadata: "CIP100:GovernanceMetadataReference",
              Other: "CIP100:OtherReference",
              label: "CIP100:reference-label",
              referenceHash: {
                "@context": {
                  hashAlgorithm: "CIP100:hashAlgorithm",
                  hashDigest: "CIP108:hashDigest",
                },
                "@id": "CIP108:referenceHash",
              },
              uri: "CIP100:reference-uri",
            },
            "@id": "CIP108:references",
          },
          title: "CIP108:title",
        },
        "@id": "CIP108:body",
      },
      hashAlgorithm: "CIP100:hashAlgorithm",
    },
    authors: [
      {
        name: "Ryan!",
        witness: {
          publicKey:
            "38f01b9b41e7ea4bca5c093e472fa01198ebaf09a55a9e97f7431c3a06df5103",
          signature:
            "29796c10ccb180c03d6053dffbbce7b6e9028dcfaf76312ebd161ed0ff40001b906b33f460ef99ed0533250a29a05d6aaab04f96aaf7c6d9acb9dfae5f5f1e0f",
          witnessAlgorithm: "ed25519",
        },
      },
    ],
    body: {
      abstract:
        "This is Ryan's proposal for a Blockchain Ecosystem Budget, which allocates 100 million ada to various projects and initiatives within the ecosystem.",
      motivation: "Without funding Ryan cannot buy an island.",
      rationale: "uhhhhhhhhhhhhhhhh",
      references: [
        {
          "@type": "Other",
          label: "Lol",
          uri: "ipfs://xd",
        },
      ],
      title: "Ryan Blockchain Ecosystem Budget - 100M ada",
    },
    hashAlgorithm: "blake2b-256",
  },
  messageHash:
    "64e9e4dcbb15d4c4aff3c47119e1c2ce025e45041c3e5c568530d2613c04f9be",
  publicKey: "38f01b9b41e7ea4bca5c093e472fa01198ebaf09a55a9e97f7431c3a06df5103",
  algorithm: "ed25519",
  signature:
    "29796c10ccb180c03d6053dffbbce7b6e9028dcfaf76312ebd161ed0ff40001b906b33f460ef99ed0533250a29a05d6aaab04f96aaf7c6d9acb9dfae5f5f1e0f",
};
