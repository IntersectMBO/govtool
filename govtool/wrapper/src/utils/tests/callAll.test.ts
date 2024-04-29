import { vi } from "vitest";

import { callAll } from "..";

describe("callAll", () => {
  it("should call all functions with provided arguments", () => {
    const fn1 = vi.fn();
    const fn2 = vi.fn();
    const fn3 = vi.fn();

    const combinedFn = callAll(fn1, fn2, fn3);
    combinedFn("arg1", "arg2");

    expect(fn1).toHaveBeenCalledWith("arg1", "arg2");
    expect(fn2).toHaveBeenCalledWith("arg1", "arg2");
    expect(fn3).toHaveBeenCalledWith("arg1", "arg2");
  });

  it("should not throw an error if any function is undefined", () => {
    const fn1 = vi.fn();
    const fn2 = undefined;
    const fn3 = vi.fn();

    const combinedFn = callAll(fn1, fn2, fn3);
    expect(() => combinedFn("arg1", "arg2")).not.toThrow();
  });
});
