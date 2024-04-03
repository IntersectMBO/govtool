import { MetadataValidationStatus } from '../enums/ValidationError';

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
};
