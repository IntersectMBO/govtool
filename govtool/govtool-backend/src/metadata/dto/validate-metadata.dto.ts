import {
  IsBoolean,
  IsEnum,
  IsNotEmpty,
  IsOptional,
  IsString,
} from 'class-validator';

import { MetadataStandard } from '../metadata.type';

export class ValidateMetadataDto {
  @IsString()
  @IsNotEmpty()
  url!: string;

  @IsString()
  @IsNotEmpty()
  hash!: string;

  @IsOptional()
  @IsEnum(MetadataStandard)
  standard?: MetadataStandard;

  /**
   * Fetch `url` now and check its bytes against `hash`, bypassing the
   * metadata service's hash cache. For submission, where the url itself goes
   * on chain and must serve the document; a read leaves it unset. A failure
   * the metadata service saw carries its `reportId` (D152).
   */
  @IsOptional()
  @IsBoolean()
  verifyUrl?: boolean;
}
