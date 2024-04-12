import { IsUrl, IsNotEmpty, IsEnum, IsOptional } from 'class-validator';

import { MetadataStandard } from '@types';

export class ValidateMetadataDTO {
  @IsNotEmpty()
  hash: string;

  @IsUrl()
  url: string;

  @IsOptional()
  @IsEnum(MetadataStandard)
  standard?: MetadataStandard;
}
