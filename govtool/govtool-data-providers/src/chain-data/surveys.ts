/**
 * Chain Data API — `/surveys/*` (SPEC.md §5.6)
 *
 * CIP-179 surveys: a survey definition is published as transaction metadata
 * under label 17, and answers are cast on chain against it. This namespace
 * returns the definition's metadata as CBOR; decoding it is the consumer's
 * job, because the payload format is versioned by the CIP, not by the chain.
 *
 * Optional on `ChainDataApiV1`: an absent namespace means the provider cannot
 * serve survey definitions. Reads are on-chain only and never fetch a URL.
 */

import type { Envelope, Hex } from './common';

export interface SurveyDefinition {
  /** The publishing transaction, lowercase hex. */
  txHash: Hex;
  /** Always 17, so a consumer never hard-codes the label. */
  metadataLabel: 17;
  /**
   * A singleton CBOR metadata map `{17: payload}`, hex, holding the complete
   * label-17 value: every survey definition the transaction publishes, not
   * one selected index. CBOR types and integer precision are preserved;
   * byte-identical serialization across providers is not required, only the
   * same decoded value.
   */
  payloadCborHex: Hex;
}

export interface SurveysApi {
  /**
   * The label-17 metadata of a transaction. `null` when the transaction does
   * not exist or carries no label 17; the two are not told apart. A source
   * that has the transaction but not its metadata bytes rejects with
   * `PROVIDER_UNAVAILABLE` rather than answering `null`.
   */
  getDefinition(txHash: string): Promise<Envelope<SurveyDefinition | null>>;
}
