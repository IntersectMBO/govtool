import { MetadataStandard } from '@types';

export class ValidateMetadataDTO {
  hash: string;

  url: string;

  standard?: MetadataStandard;
}
