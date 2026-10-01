import { MetadataValidationStatus } from './metadata-status.enum';

export enum MetadataStandard {
  /** A vote rationale; the frontend reads its `comment`. */
  CIP100 = 'CIP100',
  CIP108 = 'CIP108',
  CIP119 = 'CIP119',
}

/**
 * One rule a document breaks. An `error` makes the document unusable and comes
 * with `status: INCORRECT_FORMAT`. A `warning` alone leaves it valid, with its
 * `metadata`, so the frontend can show the content beside the warning.
 */
export type MetadataIssue = {
  field: string;
  rule: 'required' | 'maxLength';
  severity: 'error' | 'warning';
  /** `maxLength` only: the limit and the field's actual length. */
  limit?: number;
  actual?: number;
};

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
  metadata?: Record<string, unknown>;
  /** Present only when the document breaks a rule of its standard. */
  issues?: MetadataIssue[];
};
