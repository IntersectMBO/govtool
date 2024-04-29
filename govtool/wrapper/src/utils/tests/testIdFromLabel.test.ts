import { testIdFromLabel } from "..";

describe("testIdFromLabel function", () => {
  it("replaces spaces with dashes and converts the label to lowercase", () => {
    const label = "Hello World";
    const expectedTestId = "hello-world";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });

  it("handles labels with multiple spaces", () => {
    const label = "Multiple     Spaces";
    const expectedTestId = "multiple-----spaces";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });

  it("handles labels with leading and trailing spaces", () => {
    const label = "   Leading and Trailing Spaces   ";
    const expectedTestId = "leading-and-trailing-spaces";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });

  it("handles labels with special characters", () => {
    const label = "!@#$%^&*()";
    const expectedTestId = "!@#$%^&*()";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });

  it("handles empty labels", () => {
    const label = "";
    const expectedTestId = "";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });

  it("handles labels with all spaces", () => {
    const label = "   ";
    const expectedTestId = "";

    const result = testIdFromLabel(label);

    expect(result).toEqual(expectedTestId);
  });
});
