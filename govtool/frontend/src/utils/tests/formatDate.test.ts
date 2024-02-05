import { formatDisplayDate } from "..";

describe("formatDisplayDate", () => {
  it("formats a date object correctly", () => {
    const date = new Date("2023-01-01T00:00:00Z");
    const formatted = formatDisplayDate(date);
    expect(formatted).toBe("1st Jan 2023");
  });

  it("formats a date string correctly", () => {
    const dateString = "2023-01-01";
    const formatted = formatDisplayDate(dateString);
    expect(formatted).toBe("1st Jan 2023");
  });

  it("handles custom format strings", () => {
    const date = new Date("2023-12-25T00:00:00Z");
    const formatString = "yyyy-MM-dd";
    const formatted = formatDisplayDate(date, formatString);
    expect(formatted).toBe("2023-12-25");
  });
});
