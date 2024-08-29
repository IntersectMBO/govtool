import { filterUpdatableProtocolParams } from "../filterUpdatableProtocolParams";

describe("filterUpdatableProtocolParams", () => {
  it("should return only existing in reference object properties", () => {
    const originalParams = {
      id: 1,
      name: "John Doe",
      age: 30,
      email: "john.doe@example.com",
      nonce: "abc123",
    };

    const updateParams = {
      name: "Jane Doe",
      age: null,
      address: undefined,
    };

    const expectedOutput = {
      name: "John Doe",
    };

    const result = filterUpdatableProtocolParams(originalParams, updateParams);
    expect(result).toEqual(expectedOutput);
  });

  it("should return only existing in reference object properties and filter out keys", () => {
    const originalParams = {
      id: 1,
      name: "John Doe",
      age: 30,
      email: "",
      nonce: "abc123",
    };

    const updateParams = {
      id: 234,
      name: "Jane Doe",
      age: null,
      address: undefined,
    };

    const expectedOutput = {
      name: "John Doe",
    };

    const result = filterUpdatableProtocolParams(originalParams, updateParams, [
      "id",
    ]);
    expect(result).toEqual(expectedOutput);
  });

  it("should return null if original object is not provided", () => {
    const updateParams = {
      name: "Jane Doe",
      age: null,
      address: undefined,
    };

    const result = filterUpdatableProtocolParams(null, updateParams);
    expect(result).toBeNull();
  });

  it("should return null if update object is not provided", () => {
    const originalParams = {
      id: 1,
      name: "John Doe",
      age: 30,
    };

    const result = filterUpdatableProtocolParams(originalParams, null);
    expect(result).toBeNull();
  });
});
