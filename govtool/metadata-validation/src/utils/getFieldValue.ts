/**
 * Retrieves the value of a specified field from a given object.
 *
 * @param body - The object from which to retrieve the field value.
 * @param field - The name of the field to retrieve the value from.
 * @returns The value of the specified field, or undefined if the field does not exist.
 */
export const getFieldValue = (
  body: Record<string, unknown>,
  field: string,
): unknown => {
  if (body[field] && body[field]['@value']) {
    return body[field]['@value'];
  }

  if (body[field]) {
    return body[field];
  }

  return undefined;
};
