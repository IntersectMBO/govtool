import * as jsonld from 'jsonld';

/**
 * Canonizes a JSON object using jsonld.canonize.
 *
 * @param json - The JSON object to be canonized.
 * @returns A Promise that resolves to the canonized JSON object.
 */
export const canonizeJSON = async (json: Record<string, unknown>) => {
  const canonized = await jsonld.canonize(json);
  return canonized;
};
