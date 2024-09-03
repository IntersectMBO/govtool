import * as Joi from 'joi';

import { MetadataValidationStatus } from '@/enums';

import { getFieldValue } from './getFieldValue';

const CIP108FieldRules = {
  title: Joi.string().allow('').max(80),
  abstract: Joi.string().allow('').max(2500),
  motivation: Joi.string().allow(''),
  rationale: Joi.string().allow(''),
};

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

  try {
    CIP108FieldRules.title.validate(title);
    CIP108FieldRules.abstract.validate(abstract);
    CIP108FieldRules.motivation.validate(motivation);
    CIP108FieldRules.rationale.validate(rationale);

    return true;
  } catch (error) {
    throw MetadataValidationStatus.INCORRECT_FORMAT;
  }
};
