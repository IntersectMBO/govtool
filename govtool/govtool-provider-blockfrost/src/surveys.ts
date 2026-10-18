/**
 * SurveysApi over Blockfrost (SPEC.md §5.6): a transaction's CIP-179 survey
 * definition, the label-17 metadata, as CBOR. `/txs/{hash}/metadata/cbor`.
 *
 * Blockfrost answers one row per label, `{ label, metadata, cbor_metadata }`.
 * `metadata` is the hex of what db-sync stored for that label, which is the
 * singleton map `{17: payload}`; `cbor_metadata` is deprecated and never read.
 * A source that serves the bare payload instead is accepted too, and wrapped.
 * Either way the bytes are parsed and re-serialized with CSL, never rebuilt
 * from JSON, so byte strings, integer keys and 64-bit integers survive.
 *
 * A 404 (no such transaction) and a transaction without label 17 are both
 * `null`. Every other failure keeps the transport's mapping (./http).
 */
import { BigNum, GeneralTransactionMetadata, TransactionMetadatum } from '@emurgo/cardano-serialization-lib-nodejs';
import type { SurveyDefinition, SurveysApi } from '@govtool/data-providers/chain-data';

import { decodeCbor } from './cbor';
import type { Ctx } from './context';
import { internal, unavailable } from './errors';
import { parseTxHash } from './transactions';

export const SURVEY_LABEL = 17;

/** 1 MiB of CBOR, as hex. Larger is not a survey definition a node relayed. */
export const MAX_SURVEY_HEX = 2 * 1024 * 1024;

interface BfMetadataCborRow {
  label: string;
  metadata: string | null;
}

/** Anything with a `free()`, as every CSL object has. */
type Freeable = { free(): void } | undefined;

function freeAll(objects: Freeable[]): void {
  for (const o of objects) o?.free();
}

/**
 * The label-17 value as the singleton metadata map `{17: payload}`, hex.
 * `hex` is either that map already, or the payload alone; never wrapped twice.
 */
export function normalizeSurveyCbor(hex: string, txHash: string): string {
  const corrupt = (why: string) => internal(`Blockfrost returned label-17 metadata that is not valid CBOR metadata: ${why}`, { txHash });
  if (hex.length > MAX_SURVEY_HEX) throw internal('Blockfrost returned label-17 metadata over 1 MiB', { txHash, hexLength: hex.length });
  if (hex.length === 0 || !/^(?:[0-9a-fA-F]{2})+$/.test(hex)) throw corrupt('not hex');
  // CSL ignores bytes after the first item, so check the framing first: exactly
  // one well-formed CBOR item. CSL then decides whether it is metadata.
  try {
    decodeCbor(hex);
  } catch {
    throw corrupt('not exactly one CBOR item');
  }

  const owned: Freeable[] = [];
  try {
    let map: GeneralTransactionMetadata | undefined;
    try {
      map = GeneralTransactionMetadata.from_hex(hex);
      owned.push(map);
    } catch {
      map = undefined;
    }
    if (map) {
      // Already the singleton map: its only key must be 17.
      const keys = map.keys();
      owned.push(keys);
      const labels: string[] = [];
      for (let i = 0; i < keys.len(); i++) {
        const key = keys.get(i);
        owned.push(key);
        labels.push(key.to_str());
      }
      if (labels.length !== 1 || labels[0] !== String(SURVEY_LABEL)) {
        throw internal('Blockfrost returned label-17 metadata keyed by other labels', { txHash, labels });
      }
      return map.to_hex();
    }

    // The payload alone: parse it as a metadatum and wrap it.
    let value: TransactionMetadatum;
    try {
      value = TransactionMetadatum.from_hex(hex);
    } catch {
      throw corrupt('CSL rejected it');
    }
    owned.push(value);
    const wrapped = GeneralTransactionMetadata.new();
    owned.push(wrapped);
    const label = BigNum.from_str(String(SURVEY_LABEL));
    owned.push(label);
    owned.push(wrapped.insert(label, value));
    return wrapped.to_hex();
  } finally {
    freeAll(owned);
  }
}

export function createSurveysApi(ctx: Ctx): SurveysApi {
  return {
    async getDefinition(txHash) {
      const hash = parseTxHash(txHash);
      const rows = await ctx.http.getOrNull<unknown>(`/txs/${hash}/metadata/cbor`);
      if (rows === null) return ctx.envelope(null);
      if (!Array.isArray(rows)) throw internal('Blockfrost returned transaction metadata that is not a list', { txHash: hash });
      const hits = (rows as BfMetadataCborRow[]).filter(
        (row) => row !== null && typeof row === 'object' && String(row.label) === String(SURVEY_LABEL),
      );
      if (hits.length === 0) return ctx.envelope(null);
      if (hits.length > 1) throw internal('Blockfrost returned more than one label-17 metadata row', { txHash: hash, rows: hits.length });
      const { metadata } = hits[0]!;
      if (metadata === null || metadata === undefined) {
        throw unavailable('Blockfrost has the transaction but not its label-17 metadata bytes', { txHash: hash });
      }
      if (typeof metadata !== 'string') throw internal('Blockfrost returned label-17 metadata that is not a hex string', { txHash: hash });
      const definition: SurveyDefinition = {
        txHash: hash,
        metadataLabel: SURVEY_LABEL,
        payloadCborHex: normalizeSurveyCbor(metadata, hash),
      };
      return ctx.envelope(definition);
    },
  };
}
