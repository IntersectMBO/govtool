import { Injectable, Logger } from '@nestjs/common';
import { catchError, finalize, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';

import { ValidateMetadataDTO } from '@dto';
import { LoggerMessage, MetadataValidationStatus } from '@enums';
import { validateMetadataStandard, parseMetadata, getStandard } from '@utils';
import { /* MetadataStandard, */ ValidateMetadataResult } from '@types';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
    standard: paramStandard,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    let status: MetadataValidationStatus;
    let metadata: Record<string, unknown>;
    let standard = paramStandard;

    const isIPFS = url.startsWith('ipfs://');
    if (isIPFS) {
      url = `${process.env.IPFS_GATEWAY}/${url.slice(7)}`;
    }

    try {
      const { data: rawData } = await firstValueFrom(
        this.httpService
          .get(url, {
            headers: {
              // Required to not being blocked by APIs that require a User-Agent
              'User-Agent': 'GovTool/Metadata-Validation-Tool',
              'Content-Type': 'application/json',
              ...(isIPFS &&
                process.env.IPFS_PROJECT_ID && {
                  project_id: process.env.IPFS_PROJECT_ID,
                }),
            },
          })
          .pipe(
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

      // TODO: Uncomment this when gov action: 7f320409d9998712ff3a3cdf0c9439e1543f236a3d746766f78f1fdbe1e06bf8#0 expires
      // if (
      //   standard === MetadataStandard.CIP108 &&
      //   !Array.isArray(parsedData.authors)
      // ) {
      //   throw MetadataValidationStatus.INCORRECT_FORMAT;
      // }

      if (!parsedData?.body) {
        throw MetadataValidationStatus.INCORRECT_FORMAT;
      }

      if (!standard) {
        standard = getStandard(parsedData);
      }

      if (standard) {
        await validateMetadataStandard(parsedData.body, standard);
        metadata = parseMetadata(parsedData.body);
      }

      const hashedMetadata = blake.blake2bHex(rawData, undefined, 32);

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
