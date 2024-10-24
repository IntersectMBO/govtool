/**
 * Filters out null parameters from the original object and returns a new object.
 *
 * @param originalObject - The original object to filter.
 * @param filterOutKeys - An optional array of keys to exclude from the filtering process.
 * @returns The filtered object without null parameters.
 */
export const filterOutNullParams = (
  originalObject?: Record<string, unknown> | undefined | null,
  filterOutKeys?: string[],
): Record<string, unknown> | null => {
  if (!originalObject) {
    return null;
  }

  const finalObject = Object.entries(originalObject).reduce(
    (acc: Record<string, unknown>, [key, value]) => {
      if (
        value !== null &&
        value !== undefined &&
        !filterOutKeys?.includes(key)
      ) {
        if (
          typeof value === "object" &&
          !Array.isArray(value) &&
          value !== null
        ) {
          // Recursively filter the nested object
          const nestedFiltered = filterOutNullParams(
            value as Record<string, unknown>,
            filterOutKeys,
          );
          if (nestedFiltered && Object.keys(nestedFiltered).length > 0) {
            acc[key] = nestedFiltered;
          }
        } else {
          acc[key] = value;
        }
      }
      return acc;
    },
    {},
  );

  return Object.keys(finalObject).length > 0 ? finalObject : null;
};
