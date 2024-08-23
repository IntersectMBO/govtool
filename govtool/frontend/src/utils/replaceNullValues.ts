/**
 * Replaces null values in a given object with corresponding values from another object.
 *
 * @param originalValue - The original object containing the values to be replaced.
 * @param replaceValue - The object containing the replacement values.
 * @returns A new object with null values replaced by corresponding values from the original object.
 */
export const replaceNullValues = (
  originalValue?: Record<string, unknown> | null,
  replaceValue?: Record<string, unknown> | null,
) => {
  if (!originalValue || !replaceValue) return;

  const result = { ...replaceValue };

  Object.keys(replaceValue).forEach((key) => {
    if (replaceValue[key] === null) {
      result[key] = originalValue[key];
    }
  });

  return result;
};
