import { HttpException, HttpStatus, Injectable } from '@nestjs/common';
import { catchError, firstValueFrom } from 'rxjs';
import { HttpService } from '@nestjs/axios';
import * as blake from 'blakejs';

import { ValidateMetadataDTO } from './dto/validateMetadata.dto';
import { ValidationError } from './enums/ValidationError';
import { canonizeJSON } from './utils/canonizeJSON';
import { ValidateMetadataResult } from './types/validateMetadata';

@Injectable()
export class AppService {
  constructor(private readonly httpService: HttpService) {}

  async validateMetadata({
    hash,
    url,
  }: ValidateMetadataDTO): Promise<ValidateMetadataResult> {
    const { data } = await firstValueFrom(
      this.httpService.get(url).pipe(
        catchError(() => {
          throw new HttpException(
            ValidationError.URL_NOT_FOUND,
            HttpStatus.BAD_REQUEST,
          );
        }),
      ),
    );

    let canonizedMetadata;
    try {
      canonizedMetadata = await canonizeJSON(data);
    } catch (error) {
      throw new HttpException(
        ValidationError.INVALID_JSONLD,
        HttpStatus.BAD_REQUEST,
      );
    }

    const hashedMetadata = blake.blake2bHex(canonizedMetadata, undefined, 32);
    if (hashedMetadata !== hash) {
      throw new HttpException(
        ValidationError.INVALID_HASH,
        HttpStatus.BAD_REQUEST,
      );
    }

    return { hash, url, valid: true };
  }
}
