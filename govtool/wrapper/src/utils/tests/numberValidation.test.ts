import i18n from "@/i18n";
import { numberValidation } from "..";

const positiveResponse = i18n.t(
  "createGovernanceAction.fields.validations.positive",
);

const numberResponse = i18n.t(
  "createGovernanceAction.fields.validations.number",
);

describe("numberValidation function", () => {
  it("returns an error message when the input is not a valid number", () => {
    const invalidInputs = ["abc", "1.2.3", "10,000.50abc", "/"];

    invalidInputs.forEach((input) => {
      expect(numberValidation(input)).toEqual(numberResponse);
    });
  });

  it("returns an error message when the input is negative", () => {
    const negativeInputs = ["-10", "-1.5", "-5000"];

    negativeInputs.forEach((input) => {
      expect(numberValidation(input)).toEqual(positiveResponse);
    });
  });

  it("returns true when the input is a valid positive number", () => {
    const validInputs = ["10", "1.5", "5000", "10,5"];

    validInputs.forEach((input) => {
      expect(numberValidation(input)).toEqual(true);
    });
  });
});
