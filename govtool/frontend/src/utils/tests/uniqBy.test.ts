import { uniqBy } from "../uniqBy";

describe("uniqBy", () => {
  it("should return an array of unique elements based on the specified key", () => {
    const arr = [
      { id: 1, name: "John" },
      { id: 2, name: "Jane" },
      { id: 3, name: "John" },
      { id: 4, name: "Jane" },
    ];

    const result = uniqBy(arr, "name");
    expect(result).toEqual([
      { id: 3, name: "John" },
      { id: 4, name: "Jane" },
    ]);
  });

  it("should handle empty input array", () => {
    const arr: never[] = [];

    const result = uniqBy(arr, "name");

    expect(result).toEqual([]);
  });
});
