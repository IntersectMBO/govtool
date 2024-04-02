import { IsUrl, IsNotEmpty } from 'class-validator';

export class ValidateMetadataDTO {
  @IsNotEmpty()
  hash: string;

  @IsUrl()
  url: string;
}
