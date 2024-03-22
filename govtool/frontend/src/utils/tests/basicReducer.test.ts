import { basicReducer } from "../basicReducer";

describe("basicReducer", () => {
  it("should merge the previous state with the new state", () => {
    const prevState = { count: 0, name: "John" };
    const newState = { count: 1 };
    const expectedState = { count: 1, name: "John" };

    const result = basicReducer(prevState, newState);

    expect(result).toEqual(expectedState);
  });

  it("should not modify the previous state if the new state is empty", () => {
    const prevState = { count: 0, name: "John" };
    const newState = {};
    const expectedState = { count: 0, name: "John" };

    const result = basicReducer(prevState, newState);

    expect(result).toEqual(expectedState);
    expect(result).toStrictEqual(prevState);
  });
});
