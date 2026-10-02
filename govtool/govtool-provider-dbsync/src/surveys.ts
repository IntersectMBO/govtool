/**
 * SurveysApi over db-sync (SPEC.md §5.6): the label-17 metadata of a
 * transaction, as CBOR, for the consumer to decode as a CIP-179 survey.
 *
 * Read from `tx_metadata.bytes`, never rebuilt from `tx_metadata.json`: the
 * JSON rendering loses byte strings, integer map keys and integer precision.
 * db-sync writes one row per label, and each row's `bytes` is that label alone
 * as a singleton metadata map (`Map.singleton key md` in cardano-db-sync's
 * `insertTxMetadata`), so a label-17 row already is the `{17: payload}` the
 * contract asks for and is served as stored.
 *
 * A transaction db-sync has not seen and one without label 17 are both
 * `null`. A row that is not what db-sync writes (two label-17 rows for one
 * transaction, no bytes, bytes that are not a `{17: ...}` map, or an
 * implausibly large value) is corrupt source data and refused as `INTERNAL`
 * rather than served.
 */
import type { SurveyDefinition, SurveysApi } from '@govtool/data-providers/chain-data';

import type { Ctx } from './context';
import { internal } from './errors';
import { parseTxHash } from './transactions';

/**
 * `LIMIT 2` so a duplicated label-17 row shows instead of one being picked.
 * `tx_metadata.key` is a `word64type` (numeric), compared with a literal.
 */
export const SURVEY_DEFINITION_SQL = `SELECT encode(tm.bytes, 'hex') AS payload_cbor_hex
  FROM tx_metadata tm JOIN tx ON tx.id = tm.tx_id
 WHERE tx.hash = decode($1, 'hex') AND tm.key = 17
 LIMIT 2`;

export const SURVEY_METADATA_LABEL = 17;

/** CBOR map(1) (0xa1) whose key is unsigned 17 (0x11). */
const SINGLETON_LABEL_17_PREFIX = 'a111';

/** Upper bound on the stored value: 1 MiB of bytes, so 2 MiB hex characters. */
export const MAX_PAYLOAD_HEX_LENGTH = 2 * 1024 * 1024;

interface SurveyRow {
  payload_cbor_hex: string | null;
}

function checkedPayload(txHash: string, value: unknown): string {
  if (typeof value !== 'string' || value.length === 0) {
    throw internal(`db-sync label-17 metadata of ${txHash} has no bytes`);
  }
  if (value.length > MAX_PAYLOAD_HEX_LENGTH) {
    throw internal(`db-sync label-17 metadata of ${txHash} exceeds ${MAX_PAYLOAD_HEX_LENGTH / 2} bytes`);
  }
  if (value.length % 2 !== 0 || !/^[0-9a-fA-F]+$/.test(value)) {
    throw internal(`db-sync label-17 metadata of ${txHash} is not hex`);
  }
  const hex = value.toLowerCase();
  // The prefix alone is a map with a key and no value, so a payload must follow.
  if (!hex.startsWith(SINGLETON_LABEL_17_PREFIX) || hex.length === SINGLETON_LABEL_17_PREFIX.length) {
    throw internal(`db-sync label-17 metadata of ${txHash} is not a singleton {17: payload} map`);
  }
  return hex;
}

export function createSurveysApi(ctx: Ctx): SurveysApi {
  return {
    async getDefinition(txHash) {
      const hash = parseTxHash(txHash);
      const rows = await ctx.db.query<SurveyRow>(SURVEY_DEFINITION_SQL, [hash]);
      const [row] = rows;
      if (!row) return ctx.envelope<SurveyDefinition | null>(null);
      if (rows.length > 1) {
        throw internal(`db-sync holds more than one label-17 metadata row for ${hash}`);
      }
      const definition: SurveyDefinition = {
        txHash: hash,
        metadataLabel: SURVEY_METADATA_LABEL,
        payloadCborHex: checkedPayload(hash, row.payload_cbor_hex),
      };
      return ctx.envelope<SurveyDefinition | null>(definition);
    },
  };
}
