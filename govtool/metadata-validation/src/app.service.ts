import { Injectable, Logger } from '@nestjs/common';
import { catchError, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata } from '@utils';
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
        metadata = parseMetadata(data.body, standard);
      }

      const hashedMetadata = blake.blake2bHex(
        !standard ? data : metadata,
        undefined,
        32,
      );

      if (hashedMetadata !== hash) {
        throw MetadataValidationStatus.INVALID_HASH;
      }
    } catch (error) {
      Logger.error(LoggerMessage.METADATA_VALIDATION_ERROR, error);
      if (Object.values(MetadataValidationStatus).includes(error)) {
        status = error;
      }
    }

    return { status, valid: !Boolean(status), metadata };
  }
}
