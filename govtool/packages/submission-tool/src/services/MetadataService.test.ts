import { CIP_Reference } from "..";
import { MetadataService } from "./MetadataService";

describe("MetadataService", () => {
  it("should initialize jsonld and hash", async () => {
    // Arrange
    const metadataService = new MetadataService({
      cip: CIP_Reference["0108"],
      hashAlgorithm: "blake2b-256",
      body: {
        title: "123",
        abstract: "My abstract",
        motivation: "My motivation",
        rationale: "My rationale",
        references: [{ label: "some url", uri: "http://some.url" }],
      },
    });

    // Act
    await metadataService.initialize();
    const jsonld = metadataService.jsonld;
    const hash = metadataService.hash;

    // Assert
    expect(jsonld).toBeDefined();
    expect(jsonld).not.toBeNull();
    // TODO: Add structure assertions to the jsonld

    expect(hash).toBeDefined();
    expect(hash).not.toBeNull();
  });

  it("should fail on body validation", async () => {
    try {
      new MetadataService({
        cip: CIP_Reference["0108"],
        hashAlgorithm: "blake2b-256",
        body: {
          // For the testing purposes
          // @ts-expect-error
          title: 123,
          abstract: "My abstract",
          motivation: "My motivation",
          rationale: "My rationale",
          references: [{ label: "some url", uri: "http://some.url" }],
        },
      });
    } catch (error) {
      expect(error).toBeDefined();
      expect((error as any).message).toBe("Invalid metadata body");
    }
  });
});
