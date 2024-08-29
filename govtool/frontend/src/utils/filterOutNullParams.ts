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
) => {
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
        acc[key] = value;
      }
      return acc;
    },
    {},
  );

  return finalObject;
};
