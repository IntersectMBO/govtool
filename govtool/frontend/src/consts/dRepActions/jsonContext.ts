export const CIP_119 =
  "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0119/README.md#";

export const DREP_CONTEXT = {
  CIP100:
    "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0100/README.md#",
  CIP119: CIP_119,
  hashAlgorithm: "CIP100:hashAlgorithm",
  body: {
    "@id": "CIP119:body",
    "@context": {
      references: {
        "@id": "CIP119:references",
        "@container": "@set" as const,
        "@context": {
          GovernanceMetadata: "CIP100:GovernanceMetadataReference",
          Identity: "CIP119:IdentityReference",
          Link: "CIP119:LinkReference",
          Other: "CIP100:OtherReference",
          label: "CIP100:reference-label",
          uri: "CIP100:reference-uri",
          referenceHash: {
            "@id": "CIP119:referenceHash",
            "@context": {
              hashDigest: "CIP119:hashDigest",
              hashAlgorithm: "CIP100:hashAlgorithm",
            },
          },
        },
      },
      paymentAddress: "CIP119:paymentAddress",
      givenName: "CIP119:givenName",
      image: "CIP119:image",
      objectives: "CIP119:objectives",
      motivations: "CIP119:motivations",
      qualifications: "CIP119:qualifications",
      doNotList: "CIP119:doNotList",
    },
  },
  authors: {
    "@id": "CIP100:authors",
    "@container": "@set" as const,
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
};
