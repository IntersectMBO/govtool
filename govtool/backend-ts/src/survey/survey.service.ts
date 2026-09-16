import {
  BadRequestException,
  Injectable,
  NotFoundException,
} from '@nestjs/common';

import { DbService } from 'src/db/db.service';
import { SqlService } from 'src/sql/sq.service';
import {
  SurveyDefinitionResponse,
  SurveyDefinitionRow,
} from './survey.type';

@Injectable()
export class SurveyService {
  constructor(
    private readonly dbService: DbService,
    private readonly sqlService: SqlService,
  ) {}

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
    const sql = this.sqlService.load('get-survey-definition.sql');
    const result = await this.dbService.query<SurveyDefinitionRow>(sql, [
      normalizedTxId,
    ]);

    if (result.rows.length !== 1) {
      throw new NotFoundException({
        errorType: 'NotFoundError',
        message: `No metadata label 17 found for transaction ${normalizedTxId}`,
      });
    }

    return {
      txId: normalizedTxId,
      surveyIndex,
      metadataLabel: 17,
      payloadCborHex: result.rows[0].payload_cbor_hex,
    };
  }
}