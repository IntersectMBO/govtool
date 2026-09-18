import type {
  Envelope,
  SurveyDefinition,
  SurveysApi,
} from '@govtool/data-providers/chain-data';

import { invalidInput } from '../common/errors';
import { envelope } from '../common/meta';
import { runSql } from '../db/run';
import type { Queryable } from '../db/queryable';
import type { SurveyDefinitionRow } from '../rows';

const TX_HASH = /^[0-9a-fA-F]{64}$/;

export class DbSyncSurveysApi implements SurveysApi {
  constructor(private readonly db: Queryable) {}

  async getDefinition(
    txHash: string,
  ): Promise<Envelope<SurveyDefinition | null>> {
    if (!TX_HASH.test(txHash)) {
      throw invalidInput('txId must be a 64-character hex string', { txHash });
    }
    const normalized = txHash.toLowerCase();
    const rows = await runSql<SurveyDefinitionRow>(
      this.db,
      'get-survey-definition.sql',
      [normalized],
    );
    const row = rows[0];
    if (rows.length !== 1 || row === undefined) {
      return envelope(null);
    }
    return envelope({
      txHash: normalized,
      metadataLabel: 17,
      payloadCborHex: row.payload_cbor_hex,
    });
  }
}
