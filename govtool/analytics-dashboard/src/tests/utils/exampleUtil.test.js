import { sumTestExample } from "@/lib/utils";

describe("sum module", () => {
  test("adds 1 + 2 to equal 3", () => {
    expect(sumTestExample(1, 2)).toBe(3);
  });
});
