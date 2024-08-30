import { Injectable, Logger } from '@nestjs/common';
import { catchError, firstValueFrom, timeout } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';
import { AxiosRequestConfig } from 'axios';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata } from '@utils';
import { ValidateMetadataResult } from '@types';

const axiosConfig: AxiosRequestConfig = {
  timeout: 5000,
  maxContentLength: 10 * 1024 * 1024, // Max content length 10MB
  maxBodyLength: 10 * 1024 * 1024, // Max body length 10MB
};

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
    standard,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: Record<string, unknown>;

    try {
      const { data } = await firstValueFrom(
        this.httpService.get(url, axiosConfig).pipe(
          timeout(5000),
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
        JSON.stringify(data),
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
