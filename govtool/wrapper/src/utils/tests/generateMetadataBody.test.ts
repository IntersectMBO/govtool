import { CIP_108 } from "@consts";

import { generateMetadataBody } from "../generateMetadataBody";

describe("generateMetadataBody", () => {
  it("generates metadata body with filtered data", () => {
    const data = {
      name: "John Doe",
      age: 30,
      email: "johndoe@example.com",
    };
    const acceptedKeys = ["name", "age"];
    const standardReference = CIP_108;

    const result = generateMetadataBody({
      data,
      acceptedKeys,
      standardReference,
    });

    expect(result).toEqual({
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#name":
        "John Doe",
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#age": 30,
    });
  });

  it("generates metadata body with filtered data and references", () => {
    const data = {
      name: "John Doe",
      age: 30,
      email: "johndoe@example.com",
      links: [
        { link: "https://example.com/link1" },
        { link: "https://example.com/link2" },
      ],
    };
    const acceptedKeys = ["name", "age"];
    const standardReference = CIP_108;

    const result = generateMetadataBody({
      data,
      acceptedKeys,
      standardReference,
    });

    expect(result).toEqual({
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#name":
        "John Doe",
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#age": 30,
      "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#references":
        [
          {
            "@type": "Other",
            "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-label":
              "Label",
            "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-uri":
              "https://example.com/link1",
          },
          {
            "@type": "Other",
            "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-label":
              "Label",
            "https://github.com/cardano-foundation/CIPs/blob/master/CIP-0108/README.md#reference-uri":
              "https://example.com/link2",
          },
        ],
    });
  });

  it("generates metadata body with empty data", () => {
    const data = {};
    const acceptedKeys = ["name", "age"];
    const standardReference = CIP_108;

    const result = generateMetadataBody({
      data,
      acceptedKeys,
      standardReference,
    });

    expect(result).toEqual({});
  });
});
