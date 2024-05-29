import { MetadataValidationStatus } from '@enums';

export enum MetadataStandard {
  CIP108 = 'CIP108',
  CIPQQQ = 'CIPQQQ',
}

export type ValidateMetadataResult = {
  status?: MetadataValidationStatus;
  valid: boolean;
  metadata: any;
};
