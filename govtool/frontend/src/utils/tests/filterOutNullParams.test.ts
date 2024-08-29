import { filterOutNullParams } from "../filterOutNullParams";

describe("filterOutNullParams", () => {
  it("should filter out null parameters", () => {
    const originalObject = {
      key1: "value1",
      key2: null,
      key3: "value3",
      key4: null,
    };

    const expectedObject = {
      key1: "value1",
      key3: "value3",
    };

    const result = filterOutNullParams(originalObject);

    expect(result).toEqual(expectedObject);
  });

  it("should exclude keys from filtering process", () => {
    const originalObject = {
      key1: "value1",
      key2: null,
      key3: "value3",
      key4: null,
    };

    const filterOutKeys = ["key2", "key4"];

    const expectedObject = {
      key1: "value1",
      key3: "value3",
    };

    const result = filterOutNullParams(originalObject, filterOutKeys);

    expect(result).toEqual(expectedObject);
  });

  it("should return null if originalObject is null", () => {
    const originalObject = null;

    const result = filterOutNullParams(originalObject);

    expect(result).toBeNull();
  });

  it("should return null if originalObject is undefined", () => {
    const originalObject = undefined;

    const result = filterOutNullParams(originalObject);

    expect(result).toBeNull();
  });
});
