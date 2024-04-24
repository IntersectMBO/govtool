import { describe, it, expect } from "vitest";
import { canonizeJSON } from "..";

const exampleJson = {
  "@context": {
    "@language": "en-us",
    CIP100:
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
    CIP108:
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#",
    hashAlgorithm: "CIP100:hashAlgorithm",
    body: {
      "@id": "CIP108:body",
      "@context": {
        references: {
          "@id": "CIP108:references",
          "@container": "@set",
          "@context": {
            GovernanceMetadata: "CIP100:GovernanceMetadataReference",
            Other: "CIP100:OtherReference",
            label: "CIP100:reference-label",
            uri: "CIP100:reference-uri",
            referenceHash: {
              "@id": "CIP108:referenceHash",
              "@context": {
                hashDigest: "CIP108:hashDigest",
                hashAlgorithm: "CIP100:hashAlgorithm",
              },
            },
          },
        },
        title: "CIP108:title",
        abstract: "CIP108:abstract",
        motivation: "CIP108:motivation",
        rationale: "CIP108:rationale",
      },
    },
    authors: {
      "@id": "CIP100:authors",
      "@container": "@set",
      "@context": {
        name: "http://xmlns.com/foaf/0.1/name",
        witness: {
          "@id": "CIP100:witness",
          "@context": {
            witnessAlgorithm: "CIP100:witnessAlgorithm",
            publicKey: "CIP100:publicKey",
            signature: "CIP100:signature",
          },
        },
      },
    },
  },
  authors: [],
  hashAlgorithm: {
    "@value": "blake2b-256",
  },
  body: {
    abstract: {
      "@value": "Test abstract",
    },
    motivation: {
      "@value": "Test motivation",
    },
    rationale: {
      "@value": "Test rationale",
    },
    references: [
      {
        "@type": "Other",
        "CIP108:reference-label": {
          "@value": "Label",
        },
        "CIP108:reference-uri": {
          "@value": "https://www.google.com/",
        },
      },
    ],
    title: {
      "@value": "Test title",
    },
  },
};

const expectedOutput = `
_:c14n0 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#hashAlgorithm> "blake2b-256" .
_:c14n0 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#body> _:c14n2 .
_:c14n1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#OtherReference> .
_:c14n1 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-label> "Label" .
_:c14n1 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-uri> "https://www.google.com/" .
_:c14n2 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#abstract> "Test abstract" .
_:c14n2 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#motivation> "Test motivation" .
_:c14n2 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#rationale> "Test rationale" .
_:c14n2 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#references> _:c14n1 .
_:c14n2 <https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#title> "Test title" .
`
  .trim()
  .replace(/\s+\n/g, "\n");

describe("canonizeJSON", () => {
  it("should correctly canonize a jsonld object to the expected output", async () => {
    const result = await canonizeJSON(exampleJson);
    const normalizedResult = result.trim().replace(/\s+\n/g, "\n");
    expect(normalizedResult).toBe(expectedOutput);
  });
});
