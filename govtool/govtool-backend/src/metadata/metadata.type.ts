import { MetadataValidationStatus } from './metadata-status.enum';

export enum MetadataStandard {
  /** A vote rationale; the frontend reads its `comment`. */
  CIP100 = 'CIP100',
  CIP108 = 'CIP108',
  CIP119 = 'CIP119',
}

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
  metadata?: Record<string, unknown>;
};
