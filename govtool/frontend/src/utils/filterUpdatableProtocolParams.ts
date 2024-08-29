/**
 * Filters the updatable protocol parameters based on the original object,
 * reference object, and optional filter-out keys.
 *
 * @param originalObject - The original object containing the protocol parameters.
 * @param referenceObject - The reference object containing the updated protocol parameters.
 * @param filterOutKeys - An optional array of keys to be excluded from the final object.
 * @returns The filtered object containing the updatable protocol parameters or null.
 */
export const filterUpdatableProtocolParams = (
  originalObject?: Record<string, unknown> | null,
  referenceObject?: Record<string, unknown> | null,
  filterOutKeys?: string[],
): Record<string, unknown> | null => {
  if (!originalObject || !referenceObject) {
    return null;
  }

  const validKeys: string[] = Object.entries(referenceObject).reduce(
    (acc: string[], [key, value]) => {
      if (
        !filterOutKeys?.includes(key) &&
        originalObject.hasOwnProperty(key) &&
        value
      ) {
        acc.push(key);
      }
      return acc;
    },
    [],
  );

  const finalObject = Object.entries(originalObject).reduce(
    (acc: Record<string, unknown>, [key, value]) => {
      if (validKeys.includes(key)) {
        acc[key] = value;
      }
      return acc;
    },
    {},
  );

  return finalObject;
};
