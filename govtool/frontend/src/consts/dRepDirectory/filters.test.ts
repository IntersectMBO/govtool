import { DREP_DIRECTORY_FILTERS } from "./filters";

describe("DREP_DIRECTORY_FILTERS", () => {
  it("should exclude 'Yourself' from filters", () => {
    // Arrange
    const expectedFilters = [
      {
        key: "Active",
        label: "Active",
      },
      {
        key: "Inactive",
        label: "Inactive",
      },
      {
        key: "Retired",
        label: "Retired",
      },
    ];

    // Act
    const actualFilters = DREP_DIRECTORY_FILTERS;

    // Assert
    expect(actualFilters).toEqual(expectedFilters);
  });
});
