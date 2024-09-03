import { Logger } from '@nestjs/common';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { MetadataStandard } from '@types';

import { getFieldValue } from './getFieldValue';
import { validateCIP108body } from './validateCIP108body';

/**
 * Validates the metadata against a specific standard.
 * @param body - The metadata body to be validated.
 * @param standard - The metadata standard to validate against.
 * @throws {MetadataValidationStatus.INCORRECT_FORMAT} - If the metadata does not conform to the specified standard.
 */
export const validateMetadataStandard = async (
  body: Record<string, unknown>,
  standard: MetadataStandard,
) => {
  try {
    switch (standard) {
      // givenName is the only compulsory field in CIP119
      case MetadataStandard.CIP119:
        const givenName = getFieldValue(body, 'givenName');
        if (!givenName) {
          Logger.error(
            LoggerMessage.METADATA_VALIDATION_ERROR,
            MetadataValidationStatus.INCORRECT_FORMAT,
          );
          throw MetadataValidationStatus.INCORRECT_FORMAT;
        }
        return true;
      case MetadataStandard.CIP108:
        return validateCIP108body(body);
      default:
        return true;
    }
  } catch (error) {
    Logger.error(
      LoggerMessage.METADATA_VALIDATION_ERROR,
      MetadataValidationStatus.INCORRECT_FORMAT,
    );
    throw MetadataValidationStatus.INCORRECT_FORMAT;
  }
};
