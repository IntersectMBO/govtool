import { getProposalTypeNoEmptySpaces } from "..";

describe("getProposalTypeNoEmptySpaces", () => {
  it("returns correct label with no spaces for a known type", () => {
    const type = "NoConfidence";
    const expectedLabel = "NoConfidence";
    expect(getProposalTypeNoEmptySpaces(type)).toBe(expectedLabel);
  });

  it("returns correct label with no spaces for another known type", () => {
    const type = "ParameterChange";
    const expectedLabel = "ProtocolParameterChange";
    expect(getProposalTypeNoEmptySpaces(type)).toBe(expectedLabel);
  });

  it("returns the type itself with no spaces removed when no matching key is found", () => {
    const type = "UnknownType";
    expect(getProposalTypeNoEmptySpaces(type)).toBe(type);
  });

  it("returns an empty string when given an empty string", () => {
    const type = "";
    expect(getProposalTypeNoEmptySpaces(type)).toBe(type);
  });
});
