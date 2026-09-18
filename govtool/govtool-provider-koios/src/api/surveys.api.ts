import type {
  Envelope,
  SurveyDefinition,
  SurveysApi,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../common/errors';

/**
 * CIP-179 surveys are not available from Koios.
 *
 * The contract returns the label-17 metadata payload as **CBOR, hex-encoded**,
 * and is explicit about why: the payload format is versioned by the CIP, so
 * the consumer decodes it. Koios only ever exposes metadata *decoded* —
 * `/tx_metadata` returns the label's value as JSON, which has already lost
 * the CBOR's type information (map key types, definite vs. indefinite length,
 * byte strings vs. text), so it cannot be re-encoded to the bytes the survey
 * was published as.
 *
 * `/tx_cbor` does return raw bytes, but for the whole transaction. Extracting
 * the auxiliary data from it means a CBOR parser and a transaction-layout
 * decoder inside this provider — implementation the contract deliberately
 * keeps out of a provider, and a correctness risk the survey feature does not
 * justify.
 *
 * A deployment that needs surveys should keep the db-sync provider for this
 * one route; it is the only part of GovTool that has no Koios path at all.
 */
export class KoiosSurveysApi implements SurveysApi {
  getDefinition(_txHash: string): Promise<Envelope<SurveyDefinition | null>> {
    return Promise.reject(
      unsupported(
        'surveys.getDefinition',
        'Koios exposes transaction metadata as decoded JSON only; the contract requires the label-17 payload as CBOR hex',
      ),
    );
  }
}
