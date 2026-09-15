import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { validateSignature } from "../validateSignature";

describe("validateSignature", () => {
  const validCip8Input = {
    jsonContent,
    publicKey:
      "29dcd28014660e4c751a2449e7f6c7f8a89517b74a017b1ff4de4bf3b7668225",
    algorithm: "CIP-8",
    signature:
      "845829a201276761646472657373581cb567325153c5c0452811069272a8dfa1fe7ff4c9bfd5695059d4f810a166686173686564f45820c14215aed5a285b9d4f476a74c11110c25439a4310cc9688e15df9372c1bc7545840950b9b5c65f5195a877c62d8feb4f420443e822d5b5cd5a263d4045eba9c89d849d8864740f5b28b218964d7479d536416bf36f7517abfea3e5424e599a7d20b",
  };

  beforeEach(() => {
    vi.spyOn(console, "error").mockImplementation(() => {});
  });

  afterEach(() => {
    vi.restoreAllMocks();
    vi.clearAllMocks();
  });

  it("returns false if signature is missing", async () => {
    const result = await validateSignature({
      ...validCip8Input,
      signature: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if publicKey is missing", async () => {
    const result = await validateSignature({
      ...validCip8Input,
      publicKey: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if algorithm is missing", async () => {
    const result = await validateSignature({
      ...validCip8Input,
      algorithm: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false if message is missing", async () => {
    const result = await validateSignature({
      ...validCip8Input,
      jsonContent: undefined,
    });
    expect(result).toBe(false);
  });

  it("returns false for unsupported algorithm", async () => {
    vi.spyOn(console, "error").mockImplementation(() => {});
    const result = await validateSignature({
      ...validCip8Input,
      algorithm: "rsa",
    });
    expect(result).toBe(false);
    expect(console.error).toHaveBeenCalledWith("Unsupported algorithm:", "rsa");
  });

  describe("CIP-8 algorithm", () => {
    it("accepts 'CIP-8' (case-insensitive)", async () => {
      const result = await validateSignature({
        ...validCip8Input,
        algorithm: "CIP-8",
      });
      expect(result).toBe(true);
    });

    it("accepts 'CIP-0008' (case-insensitive)", async () => {
      const result = await validateSignature({
        ...validCip8Input,
        algorithm: "CIP-0008",
      });
      expect(result).toBe(true);
    });

    it("returns false for different json content", async () => {
      const invalidCip8Input = {
        ...validCip8Input,
        jsonContent: { test: "Hello" },
      };
      const result = await validateSignature(invalidCip8Input);
      expect(result).toBe(false);
    });
  });
});

const jsonContent = {
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
      name: "mike",
      witness: {
        publicKey:
          "38f01b9b41e7ea4bca5c093e472fa01198ebaf09a55a9e97f7431c3a06df5103",
        signature:
          "e03643f6f73ebb6edcdedef62e55d132d7a0db11b1ca27b62992ba028f0290e21835898a06fe899746b45682c275ecd82150429401b4a97ebb3119239c70960b",
        witnessAlgorithm: "ed25519",
      },
    },
    {
      name: "bsok",
      witness: {
        publicKey:
          "29dcd28014660e4c751a2449e7f6c7f8a89517b74a017b1ff4de4bf3b7668225",
        signature:
          "845829a201276761646472657373581cb567325153c5c0452811069272a8dfa1fe7ff4c9bfd5695059d4f810a166686173686564f45820c14215aed5a285b9d4f476a74c11110c25439a4310cc9688e15df9372c1bc7545840950b9b5c65f5195a877c62d8feb4f420443e822d5b5cd5a263d4045eba9c89d849d8864740f5b28b218964d7479d536416bf36f7517abfea3e5424e599a7d20b",
        witnessAlgorithm: "CIP-0008",
      },
    },
  ],
  body: {
    abstract: "My amazing epic metadata",
    motivation: "😆😆😆😆😆😆",
    rationale: "🚀",
    references: [
      {
        "@type": "Other",
        label: "Ratification Methodology",
        uri: "ipfs://bafkreiagfwdg3iejt5wpks5cwm35kibpana7zdbupn2xij4jrc33ugf6gm",
      },
    ],
    title: "Many authors test",
  },
  hashAlgorithm: "blake2b-256",
};
