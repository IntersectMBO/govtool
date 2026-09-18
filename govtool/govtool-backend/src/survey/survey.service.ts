import {
  BadRequestException,
  Inject,
  Injectable,
  NotFoundException,
} from '@nestjs/common';
import type { ChainDataApiV1 } from '@govtool/data-providers/chain-data';

import { asHttp } from 'src/common/errors';
import { CHAIN_DATA } from 'src/providers/providers.module';
import { SurveyDefinitionResponse } from './survey.type';

@Injectable()
export class SurveyService {
  constructor(@Inject(CHAIN_DATA) private readonly chain: ChainDataApiV1) {}

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

    const definition = await asHttp(async () => {
      // `surveys` is optional on the contract: a provider with no CIP-179
      // statement omits the namespace entirely.
      if (this.chain.surveys === undefined) {
        return null;
      }
      const { data } = await this.chain.surveys.getDefinition(normalizedTxId);
      return data;
    });

    if (definition === null) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `No metadata label 17 found for transaction ${normalizedTxId}`,
      });
    }

    return {
      txId: normalizedTxId,
      // Not read from chain: the index addresses a question inside the
      // payload, which the consumer decodes.
      surveyIndex,
      metadataLabel: 17,
      payloadCborHex: definition.payloadCborHex,
    };
  }
}
