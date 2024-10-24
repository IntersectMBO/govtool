import { Injectable, Logger } from '@nestjs/common';
import { catchError, finalize, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';
import * as jsonld from 'jsonld';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { ValidateMetadataResult } from '@types';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: Record<string, unknown>;

    try {
      const { data: rawData } = await firstValueFrom(
        this.httpService.get(url).pipe(
          finalize(() => Logger.log(`Fetching ${url} completed`)),
          catchError((error) => {
            Logger.error(error, JSON.stringify(error));
            throw MetadataValidationStatus.URL_NOT_FOUND;
          }),
        ),
      );

      let parsedData;
      try {
        parsedData = JSON.parse(rawData);
      } catch (error) {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      if (!parsedData?.body) {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      const standard = getStandard(parsedData);

      if (standard) {
        await validateMetadataStandard(parsedData.body, standard);
        metadata = parseMetadata(parsedData.body);
      }

      const hashedMetadata = blake.blake2bHex(rawData, undefined, 32);

      if (hashedMetadata !== hash) {
        // Optionally validate on a parsed metadata
        const hashedParsedMetadata = blake.blake2bHex(
          JSON.stringify(parsedData, null, 2),
          undefined,
          32,
        );
        if (hashedParsedMetadata !== hash) {
          // Optional support for the canonized data hash
          // Validate canonized data hash
          const canonizedMetadata = await jsonld.canonize(JSON.parse(rawData), {
            safe: false,
          });

          const hashedCanonizedMetadata = blake.blake2bHex(
            canonizedMetadata,
            undefined,
            32,
          );

          if (hashedCanonizedMetadata !== hash) {
            throw MetadataValidationStatus.INVALID_HASH;
          }
        }
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
