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

  const finalObject = Object.entries(referenceObject).reduce<
    Record<string, unknown>
  >((acc, [key, referenceValue]) => {
    const originalValue = originalObject[key];

    const isValid =
      !filterOutKeys?.includes(key) &&
      originalObject.hasOwnProperty(key) &&
      referenceValue !== undefined &&
      referenceValue !== null;

    if (isValid) {
      if (
        typeof originalValue === "object" &&
        originalValue !== null &&
        typeof referenceValue === "object" &&
        referenceValue !== null
      ) {
        const nestedFiltered = filterUpdatableProtocolParams(
          originalValue as Record<string, unknown>,
          referenceValue as Record<string, unknown>,
          filterOutKeys,
        );
        if (nestedFiltered && Object.keys(nestedFiltered).length > 0) {
          acc[key] = nestedFiltered;
        }
      } else {
        acc[key] = originalValue;
      }
    }

    return acc;
  }, {});

  return Object.keys(finalObject).length > 0 ? finalObject : null;
};
