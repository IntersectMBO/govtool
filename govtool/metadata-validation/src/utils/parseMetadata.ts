import { getFieldValue } from './getFieldValue';

/**
 * Parses the metadata from the given body object.
 *
 * @param body - The body object containing the metadata.
 * @returns An object with the parsed metadata.
 */
export const parseMetadata = (body: Record<string, unknown>) => {
  const metadata = {};

  Object.keys(body).forEach((key) => {
    metadata[key] = getFieldValue(body, key);
  });
  return metadata;
};
