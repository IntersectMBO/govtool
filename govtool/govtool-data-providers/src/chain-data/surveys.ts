/**
 * Chain Data API — `/surveys/*`
 *
 * CIP-179 surveys: a survey definition is published as transaction metadata
 * under label 17, and its answers are cast on chain against it. This module
 * returns the definition's raw CBOR payload; decoding it is the consumer's job,
 * because the payload format is versioned by the CIP, not by the chain.
 *
 * The namespace is optional on `ChainDataApiV1`: it is GovTool-specific, and a
 * general-purpose provider has no reason to implement it.
 */

import type { Envelope, Hex } from './common';

export interface SurveyDefinition {
  txHash: Hex;
  /** Always 17 — kept on the record so a consumer never hard-codes the label. */
  metadataLabel: 17;
  /** The metadata payload, CBOR, hex-encoded. Immutable for a given `txHash`. */
  payloadCborHex: Hex;
}

export interface SurveysApi {
  /**
   * `GET /surveys/{txHash}` — `null` when the transaction exists but carries
   * no label-17 metadata, or does not exist at all; the two are not
   * distinguishable from the metadata table alone.
   */
  getDefinition(txHash: string): Promise<Envelope<SurveyDefinition | null>>;
}
