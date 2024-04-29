import {
  getItemFromLocalStorage,
  setItemToLocalStorage,
  removeItemFromLocalStorage,
} from "..";

const EXAMPLE_KEY = "example_key";
const VALUE = "exampleValue";

describe("localStorage util", () => {
  it("returns correctly value after set item to localstorage", () => {
    setItemToLocalStorage(EXAMPLE_KEY, VALUE);

    const itemFromStorage = getItemFromLocalStorage(EXAMPLE_KEY);

    expect(itemFromStorage).toBe(VALUE);
  });

  it("returns null after remove item from localstorage", () => {
    setItemToLocalStorage(EXAMPLE_KEY, VALUE);
    removeItemFromLocalStorage(EXAMPLE_KEY);

    const itemFromStorage = getItemFromLocalStorage(EXAMPLE_KEY);

    expect(itemFromStorage).toBe(null);
  });
});
