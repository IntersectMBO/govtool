import { getProposalTypeLabel } from "..";

describe("getProposalTypeLabel", () => {
  it("returns correct label for a known type", () => {
    const type = "NoConfidence";
    const expectedLabel = "No Confidence";
    expect(getProposalTypeLabel(type)).toBe(expectedLabel);
  });

  it("returns correct label for another known type", () => {
    const type = "ParameterChange";
    const expectedLabel = "Protocol Parameter Change";
    expect(getProposalTypeLabel(type)).toBe(expectedLabel);
  });

  it("returns the type itself when no matching key is found", () => {
    const type = "UnknownType";
    expect(getProposalTypeLabel(type)).toBe(type);
  });

  it("returns the type itself when given an empty string", () => {
    const type = "";
    expect(getProposalTypeLabel(type)).toBe(type);
  });
});
