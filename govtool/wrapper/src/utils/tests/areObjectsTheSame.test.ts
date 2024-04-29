import { areObjectsTheSame } from "..";

const objectOne = {
  prop1: "prop1",
  prop2: "prop2",
};

const objectWithDifferentLengths = {
  prop1: "prop1",
  prop2: "prop2",
  prop3: "prop3",
};

const objectWithDifferentProperties = {
  prop1: "prop1",
  prop3: "prop3",
};

const sameObjectAsObjectOne = {
  prop1: "prop1",
  prop2: "prop2",
};

describe("areObjectsTheSame", () => {
  it("should return false when objects have different lengths", () => {
    const result = areObjectsTheSame(objectOne, objectWithDifferentLengths);

    expect(result).toEqual(false);
  });

  it("should return false when objects have different properties", () => {
    const result = areObjectsTheSame(objectOne, objectWithDifferentProperties);

    expect(result).toEqual(false);
  });

  it("should return true when objects are the same", () => {
    const result = areObjectsTheSame(objectOne, sameObjectAsObjectOne);

    expect(result).toEqual(true);
  });
});
