import { Injectable } from '@nestjs/common';
import { catchError, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';

import { ValidateMetadataDTO } from './dto/validateMetadata.dto';
import { MetadataValidationStatus } from './enums/ValidationError';
import { canonizeJSON } from './utils/canonizeJSON';
import { ValidateMetadataResult } from './types/validateMetadata';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    try {
      const { data } = await firstValueFrom(
        this.httpService.get(url).pipe(
          catchError(() => {
            throw MetadataValidationStatus.URL_NOT_FOUND;
          }),
        ),
      );

      let canonizedMetadata;
      try {
        canonizedMetadata = await canonizeJSON(data);
      } catch (error) {
        throw MetadataValidationStatus.INVALID_JSONLD;
      }

      const hashedMetadata = blake.blake2bHex(canonizedMetadata, undefined, 32);
      if (hashedMetadata !== hash) {
        throw MetadataValidationStatus.INVALID_HASH;
      }
    } catch (error) {
      if (Object.values(MetadataValidationStatus).includes(error)) {
        status = error;
      }
    }

    return { status, valid: !Boolean(status) };
  }
}
