import { Injectable } from '@nestjs/common';
import { catchError, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';

import { ValidateMetadataDTO } from '@dto';
import { MetadataValidationStatus } from '@enums';
import { canonizeJSON, validateMetadataStandard, parseMetadata } from '@utils';
import { ValidateMetadataResult } from '@types';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
    standard,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: any;
    try {
      const { data } = await firstValueFrom(
        this.httpService.get(url).pipe(
          catchError(() => {
            throw MetadataValidationStatus.URL_NOT_FOUND;
          }),
        ),
      );

      if (standard) {
        await validateMetadataStandard(data, standard);
      }

      metadata = parseMetadata(data.body, standard);

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

    return { status, valid: !Boolean(status), metadata };
  }
}
