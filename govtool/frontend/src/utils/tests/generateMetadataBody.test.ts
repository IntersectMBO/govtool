import { generateMetadataBody } from "../generateMetadataBody";

describe("generateMetadataBody", () => {
  it("generates metadata body with filtered data", async () => {
    const data = {
      name: "John Doe",
      age: 30,
      email: "johndoe@example.com",
    };
    const acceptedKeys = ["name", "age"];

    const result = await generateMetadataBody({
      data,
      acceptedKeys,
    });

    expect(result).toEqual({
      name: "John Doe",
      age: 30,
    });
  });

  it("generates metadata body with filtered data and references", async () => {
    const data = {
      name: "John Doe",
      age: 30,
      email: "johndoe@example.com",
      references: [
        { uri: "https://example.com/link1" },
        { uri: "https://example.com/link2" },
      ],
    };
    const acceptedKeys = ["name", "age"];

    const result = await generateMetadataBody({
      data,
      acceptedKeys,
    });

    expect(result).toEqual({
      name: "John Doe",
      age: 30,
      references: [
        {
          "@type": "Other",
          label: "Label",
          uri: "https://example.com/link1",
        },
        {
          "@type": "Other",
          label: "Label",
          uri: "https://example.com/link2",
        },
      ],
    });
  });

  it("generates metadata body with empty data", async () => {
    const data = {};
    const acceptedKeys = ["name", "age"];

    const result = await generateMetadataBody({
      data,
      acceptedKeys,
    });

    expect(result).toEqual({});
  });
});
