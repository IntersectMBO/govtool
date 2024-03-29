import { Injectable } from '@nestjs/common';
import { ValidateMetadataDTO } from './dto/validateMetadata.dto';

@Injectable()
export class AppService {
  validateMetadata(
    validateMetadataDto: ValidateMetadataDTO,
  ): ValidateMetadataDTO {
    return validateMetadataDto;
  }
}
