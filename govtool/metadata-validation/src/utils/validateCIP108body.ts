import { MetadataValidationStatus } from '@/enums';

import { getFieldValue } from './getFieldValue';

/**
 * Validates the body of a CIP108 standard.
 *
 * @param body - The body of the metadata.
 * @returns True if the body is valid, otherwise throws an error.
 * @throws {MetadataValidationStatus} - Throws an error if the body is not in the correct format.
 */
export const validateCIP108body = (body: Record<string, unknown>) => {
  const title = getFieldValue(body, 'title');
  const abstract = getFieldValue(body, 'abstract');
  const motivation = getFieldValue(body, 'motivation');
  const rationale = getFieldValue(body, 'rationale');
  if (!title || !abstract || !motivation || !rationale) {
    throw MetadataValidationStatus.INCORRECT_FORMAT;
  }
  if (String(title).length > 80 || String(abstract).length > 2500) {
    throw MetadataValidationStatus.INCORRECT_FORMAT;
  }

  return true;
};
