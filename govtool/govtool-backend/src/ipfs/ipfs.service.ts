import { HttpException, Inject, Injectable } from '@nestjs/common';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';

import { asHttp } from 'src/common/errors';
import { PINNING } from 'src/providers/providers.module';
import { UploadResponse } from './ipfs.type';

@Injectable()
export class IpfsService {
  constructor(
    @Inject(PINNING) private readonly pinning: PinningServiceV1 | null,
  ) {}

  /**
   * The legacy upload endpoint. Size and backend failures are classified by
   * the pinning service and mapped back to the legacy `errorType` names in
   * `src/common/errors.ts`, so the response bodies are unchanged.
   */
  async upload(fileName: string, fileContent: string): Promise<UploadResponse> {
    if (this.pinning === null) {
      throw new HttpException(
        {
          errorType: 'IpfsUnconfiguredError',
          message: 'Backend is not configured for ipfs upload',
        },
        503,
      );
    }

    return asHttp(async () => {
      const pin = await this.pinning!.pin({
        content: fileContent,
        // The legacy endpoint sent the body as text/plain regardless of what
        // the document actually was; the on-chain hash is over these bytes
        // either way, so the type is kept as-is.
        contentType: 'text/plain',
        fileName,
      });
      return { ipfsCid: pin.cid };
    });
  }
}
