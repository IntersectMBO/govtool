/**
 * SurveysApi over Koios (SPEC.md §5.6): the label-17 metadata of a CIP-179
 * survey transaction, as CBOR.
 *
 * Read from the transaction's own bytes (`/tx_cbor`), never from
 * `/tx_metadata`: Koios renders metadata as JSON, which loses byte strings,
 * integer map keys and integer precision, so it cannot be turned back into
 * the CBOR value. Before anything is read, the body's bytes must hash to the
 * transaction asked for, and the auxiliary data's bytes to the hash the body
 * declares for them, so a Koios instance cannot serve metadata the chain does
 * not hold. Only the auxiliary data, once its nesting is bounded, is decoded
 * with CSL (see ./cbor).
 *
 * `/tx_cbor` answers `cbor: null` when the instance does not retain
 * transaction bytes: that is PROVIDER_UNAVAILABLE, not "no survey".
 */
import { AuxiliaryData, BigNum, GeneralTransactionMetadata } from '@emurgo/cardano-serialization-lib-nodejs';
import { ChainDataError, type SurveysApi } from '@govtool/data-providers/chain-data';
import { blake2bHex } from 'blakejs';

import { auxiliaryDataHash, CborError, MAX_DECODED_DEPTH, transactionSpans, type TransactionSpans } from './cbor';
import type { Ctx } from './context';
import { internal, unavailable } from './errors';
import { isHex } from './ids';
import type { TxCborRow } from './rows';
import { parseTxHash } from './transactions';

export const SURVEY_METADATA_LABEL = 17;

/** A transaction is at most 16 KiB on chain today; anything over 1 MiB is not one. */
export const MAX_TX_CBOR_BYTES = 1024 * 1024;

/**
 * The label-17 value of a transaction's CBOR as a singleton metadata map
 * `{17: payload}`, hex; `null` when the transaction carries no label 17.
 * Throws PROVIDER_UNAVAILABLE when the bytes are missing and INTERNAL when
 * they are oversized, malformed, nested too deeply to decode, belong to
 * another transaction, or carry auxiliary data the body does not commit to.
 */
export function label17FromTxCbor(txHash: string, cbor: unknown): string | null {
  if (cbor === null || cbor === undefined || (typeof cbor === 'string' && cbor.trim() === '')) {
    throw unavailable('Koios has the transaction but not its CBOR (not retained by this instance)', { txHash });
  }
  if (typeof cbor !== 'string') throw internal('Koios returned transaction CBOR that is not a string', { txHash });
  const hex = cbor.trim();
  if (hex.length > MAX_TX_CBOR_BYTES * 2) {
    throw internal('Koios returned transaction CBOR over the size bound', { txHash, bytes: Math.ceil(hex.length / 2) });
  }
  if (!isHex(hex)) throw internal('Koios returned transaction CBOR that is not hex', { txHash });

  let spans: TransactionSpans;
  let declaredAuxHash: string | null;
  try {
    spans = transactionSpans(Buffer.from(hex, 'hex'));
    declaredAuxHash = auxiliaryDataHash(spans.body);
  } catch (cause) {
    if (cause instanceof CborError) throw internal('Koios returned transaction CBOR that does not decode', { txHash }, cause);
    throw cause;
  }
  const actual = blake2bHex(spans.body, undefined, 32);
  if (actual !== txHash) {
    throw internal('Koios returned the CBOR of a different transaction', { txHash, decodedTxHash: actual });
  }
  const aux = spans.auxiliaryData;
  const auxHash = aux === null ? null : blake2bHex(aux.bytes, undefined, 32);
  if (auxHash !== declaredAuxHash) {
    throw internal('Koios returned auxiliary data that does not match the transaction body', {
      txHash,
      declaredAuxiliaryDataHash: declaredAuxHash,
      auxiliaryDataHash: auxHash,
    });
  }
  if (aux === null) return null;
  if (aux.depth > MAX_DECODED_DEPTH) {
    throw internal('Koios returned auxiliary data nested too deeply to decode', { txHash, depth: aux.depth });
  }

  // Every CSL object is a handle into wasm memory; each one made here is freed.
  const owned: { free(): void }[] = [];
  const own = <T extends { free(): void } | undefined>(value: T): T => {
    if (value !== undefined) owned.push(value);
    return value;
  };
  try {
    let auxiliaryData: AuxiliaryData;
    try {
      auxiliaryData = own(AuxiliaryData.from_bytes(aux.bytes));
    } catch (cause) {
      throw internal('Koios returned auxiliary data that does not decode', { txHash }, cause);
    }
    const metadata = own(auxiliaryData.metadata());
    if (metadata === undefined) return null;
    const label = own(BigNum.from_str(String(SURVEY_METADATA_LABEL)));
    const payload = own(metadata.get(label));
    if (payload === undefined) return null;
    const singleton = own(GeneralTransactionMetadata.new());
    own(singleton.insert(label, payload));
    return singleton.to_hex();
  } catch (error) {
    if (error instanceof ChainDataError) throw error;
    throw internal('could not read label 17 from the transaction CBOR', { txHash }, error);
  } finally {
    for (const o of owned) o.free();
  }
}

export function createSurveysApi(ctx: Ctx): SurveysApi {
  return {
    async getDefinition(txHash) {
      const hash = parseTxHash(txHash);
      const { rows } = await ctx.http.post<TxCborRow>('tx_cbor', { _tx_hashes: [hash] }, undefined, { select: 'tx_hash,cbor' });
      if (rows.length === 0) return ctx.envelope(null);
      const row = rows.find((r) => typeof r?.tx_hash === 'string' && r.tx_hash.toLowerCase() === hash);
      if (!row) throw internal('Koios answered /tx_cbor for a different transaction', { txHash: hash });
      const payloadCborHex = label17FromTxCbor(hash, row.cbor);
      return ctx.envelope(
        payloadCborHex === null ? null : { txHash: hash, metadataLabel: SURVEY_METADATA_LABEL, payloadCborHex },
      );
    },
  };
}
