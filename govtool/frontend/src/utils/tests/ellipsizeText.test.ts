import { ellipsizeText } from "../ellipsizeText";

describe("ellipsizeText", () => {
  it("should return the original text if it is shorter than or equal to maxLength", () => {
    const text = "Hello, World!";
    const maxLength = 20;

    const result = ellipsizeText(text, maxLength);

    expect(result).toEqual(text);
  });

  it("should return the ellipsized text if it is longer than maxLength", () => {
    const text = "This is a long text that needs to be ellipsized.";
    const maxLength = 8;
    const expected = "This is...";

    const result = ellipsizeText(text, maxLength);

    expect(result).toEqual(expected);
  });
});
