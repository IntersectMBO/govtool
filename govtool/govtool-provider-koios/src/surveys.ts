/**
 * SurveysApi over Koios (SPEC.md §5.6): the label-17 metadata of a CIP-179
 * survey transaction, as CBOR.
 *
 * Read from the transaction's own bytes (`/tx_cbor`), never from
 * `/tx_metadata`: Koios renders metadata as JSON, which loses byte strings,
 * integer map keys and integer precision, so it cannot be turned back into
 * the CBOR value. The transaction is decoded with `FixedTransaction`, which
 * keeps the original body bytes, so its hash is checked against the one asked
 * for before anything is read from it.
 *
 * `/tx_cbor` answers `cbor: null` when the instance does not retain
 * transaction bytes: that is PROVIDER_UNAVAILABLE, not "no survey".
 */
import { BigNum, FixedTransaction, GeneralTransactionMetadata } from '@emurgo/cardano-serialization-lib-nodejs';
import { ChainDataError, type SurveysApi } from '@govtool/data-providers/chain-data';

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
 * they are oversized, malformed or belong to another transaction.
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

  // Every CSL object is a handle into wasm memory; each one made here is freed.
  const owned: { free(): void }[] = [];
  const own = <T extends { free(): void } | undefined>(value: T): T => {
    if (value !== undefined) owned.push(value);
    return value;
  };
  try {
    let tx: FixedTransaction;
    try {
      tx = own(FixedTransaction.from_hex(hex));
    } catch (cause) {
      throw internal('Koios returned transaction CBOR that does not decode', { txHash }, cause);
    }
    const actual = own(tx.transaction_hash()).to_hex();
    if (actual !== txHash) {
      throw internal('Koios returned the CBOR of a different transaction', { txHash, decodedTxHash: actual });
    }
    const metadata = own(own(tx.auxiliary_data())?.metadata());
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
