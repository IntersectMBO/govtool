/**
 * Maps an object to a new object by including only the desired keys
 * and converting arrays to objects.
 * @param obj - The object to map.
 * @param desiredKeys - An array of keys to include in the mapped object.
 * @returns The mapped object.
 */
export const mapArrayToObjectByKeys = (
  obj?: Record<string, unknown> | null,
  desiredKeys?: string[],
): Record<string, unknown> | null => {
  if (!obj || !desiredKeys) {
    return null;
  }

  return Object.entries(obj).reduce((acc, [key, value]) => {
    if (desiredKeys.includes(key) && Array.isArray(value)) {
      const arrayToObject = value.reduce<Record<string, unknown>>(
        (arrayAcc, arrayValue, index) => {
          arrayAcc[index] = arrayValue;
          return arrayAcc;
        },
        {},
      );
      acc[key] = arrayToObject;
    } else if (
      typeof value === "object" &&
      value !== null &&
      !Array.isArray(value)
    ) {
      acc[key] = mapArrayToObjectByKeys(
        value as Record<string, unknown>,
        desiredKeys,
      );
    } else {
      acc[key] = value;
    }
    return acc;
  }, {} as Record<string, unknown>);
};
