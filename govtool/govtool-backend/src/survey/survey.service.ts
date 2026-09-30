import { BadRequestException, Injectable } from '@nestjs/common';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import { asHttp } from 'src/common/errors';
import { SurveyDefinitionResponse } from './survey.type';

@Injectable()
export class SurveyService {
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

    // CIP-179 survey definitions are transaction metadata, not governance
    // state, and no provider serves them any more. The route and its argument
    // checks stay — a malformed request is still a 400 — but a well-formed one
    // now says the capability is gone rather than inventing a payload.
    void surveyIndex;
    return asHttp(() =>
      Promise.reject(
        new ChainDataError(
          'CAPABILITY_UNSUPPORTED',
          `No survey definition source is configured, so metadata label 17 for transaction ${normalizedTxId} cannot be read.`,
        ),
      ),
    );
  }
}
