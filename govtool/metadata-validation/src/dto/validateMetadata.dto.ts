import { IsEnum } from 'class-validator';

import { MetadataStandard } from '@types';

export class ValidateMetadataDTO {
  hash: string;

  url: string;

  @IsEnum(MetadataStandard, { message: 'Invalid metadata standard' })
  standard?: MetadataStandard;
}
