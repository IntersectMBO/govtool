import { HttpException, Inject, Injectable } from '@nestjs/common';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';

import { asHttp } from 'src/common/errors';
import { PINNING } from 'src/providers/providers.module';
import { UploadResponse } from './ipfs.type';

/**
 * `pinData` takes the owner a quota is counted against. The legacy route is
 * unauthenticated and carries no wallet identity, so every legacy upload is
 * attributed to the backend itself.
 */
const LEGACY_UPLOAD_OWNER = 'govtool-legacy-upload';

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
      // The legacy endpoint sent the body as text/plain regardless of what the
      // document actually was; the on-chain hash is over these bytes either
      // way, so they are pinned exactly as received. `fileName` is no longer
      // carried: the contract pins bytes, not a named file.
      void fileName;
      const cid = await this.pinning!.pinData(
        Buffer.from(fileContent, 'utf8'),
        LEGACY_UPLOAD_OWNER,
      );
      return { ipfsCid: cid };
    });
  }
}
