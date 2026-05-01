import { HttpException, Injectable } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';
import { PinataUploadResponse, UploadResponse } from './ipfs.type';

@Injectable()
export class IpfsService {
  constructor(private readonly configSerivce: ConfigService) {}

  async upload(fileName: string, fileContent: string): Promise<UploadResponse> {
    const size = Buffer.byteLength(fileContent, 'utf8');

    if (size > 1024 * 512) {
      throw new HttpException(
        {
          errorType: 'ValidationError',
          message: 'The uploaded file is larger than 500kb',
        },
        400,
      );
    }

    const jwt = this.configSerivce.get().pinataApiJwt;

    if (!jwt) {
      throw new HttpException(
        {
          errorType: 'IpfsUnconfiguredError',
          message: 'Backend is not configured for ipfs upload',
        },
        503,
      );
    }

    const formData = new FormData();
    formData.append('network', 'public');
    formData.append(
      'file',
      new Blob([fileContent], { type: 'text/plain' }),
      fileName,
    );

    let response: Response;

    try {
      response = await fetch('https://upload.pinata.cloud/v3/files', {
        method: 'POST',
        headers: {
          Authorization: `Bearer ${jwt}`,
        },
        body: formData,
      });
    } catch (error) {
      throw new HttpException(
        {
          errorType: 'PinataConenctionError',
          message: String(error),
        },
        503,
      );
    }

    const responseText = await response.text();

    if (!response.ok) {
      throw new HttpException(
        {
          errorType: 'PinataAPIError',
          message: `Pinata API returned error status : ${response.status}`,
          pinataResponse: {
            status: String(response.status),
            body: responseText,
          },
        },
        503,
      );
    }

    let parsed: PinataUploadResponse;

    try {
      parsed = JSON.parse(responseText) as PinataUploadResponse;
    } catch {
      throw new HttpException(
        {
          errorType: 'PinataDecodingError',
          message: 'Failed to decode Pinata API reponse',
          pinataResponse: {
            status: 'unknown',
            body: responseText,
          },
        },
        503,
      );
    }

    if (!parsed.data?.cid) {
      throw new HttpException(
        {
          errorType: 'PinataDecodingError',
          message: 'Failed to decode Pinata API reponse',
          pinataResponse: {
            status: 'unknown',
            body: responseText,
          },
        },
        503,
      );
    }

    return {
      ipfsCid: parsed.data.cid,
    };
  }
}
