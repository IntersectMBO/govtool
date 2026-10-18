import {
  BadRequestException,
  Inject,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { CacheService } from 'src/cache/cache.service';
import { asHttp, required } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';

import { SurveyDefinitionResponse } from './survey.type';

/** How long a definition read is kept; it is on chain, but a rollback can undo it. */
export const SURVEY_CACHE_SECONDS = 60;

@Injectable()
export class SurveyService {
  constructor(
    @Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1,
    private readonly cacheService: CacheService,
  ) {}

  /**
   * The label-17 metadata of a transaction, as the CIP-179 definition the
   * frontend decodes. The whole batch is returned for any syntactically valid
   * index: whether that definition exists is the frontend's to decide.
   */
  async getDefinition(
    txId: string,
    rawSurveyIndex: string,
  ): Promise<SurveyDefinitionResponse> {
    if (!/^[0-9a-fA-F]{64}$/.test(txId)) {
      throw new BadRequestException({
        errorType: 'ValidationError',
        message: 'txId must be a 64-character hex string',
      });
    }

    if (!/^\d+$/.test(rawSurveyIndex)) {
      throw new BadRequestException({
        errorType: 'ValidationError',
        message: 'surveyIndex must be between 0 and 65535',
      });
    }

    const surveyIndex = Number(rawSurveyIndex);

    if (!Number.isSafeInteger(surveyIndex) || surveyIndex > 65535) {
      throw new BadRequestException({
        errorType: 'ValidationError',
        message: 'surveyIndex must be between 0 and 65535',
      });
    }

    const normalizedTxId = txId.toLowerCase();
    // Keyed by the transaction alone: every index reads the same batch.
    // Concurrent reads share one provider call, and a failure (a missing
    // definition included) is not kept, so a retry asks again.
    const payloadCborHex = await this.cacheService.getOrSet(
      'surveyDefinition',
      normalizedTxId,
      () =>
        asHttp(async () => {
          const surveys = required(this.chain.surveys, 'surveys.getDefinition');
          const { data } = await surveys.getDefinition(normalizedTxId);
          if (data === null) {
            throw new NotFoundException({
              errorType: 'NotFoundError',
              message: `No metadata label 17 found for transaction ${normalizedTxId}`,
            });
          }
          return data.payloadCborHex;
        }),
      SURVEY_CACHE_SECONDS,
    );

    return {
      txId: normalizedTxId,
      surveyIndex,
      metadataLabel: 17,
      payloadCborHex,
    };
  }
}
