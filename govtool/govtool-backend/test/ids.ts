/**
 * Real, decodable identifiers for the specs.
 *
 * The services translate legacy ids into the contract's forms and back, so a
 * placeholder like `drep1cip129` no longer survives a round trip: every id a
 * stub hands out has to be one a conforming provider would.
 */
import {
  encodeCip129DRepId,
  encodeCip129GovActionId,
} from '../src/common/legacy-ids';

/** A 28-byte hash from a one-character seed: `hash('a')` is 56 × 'a'. */
export function hash(seed: string): string {
  return seed.repeat(56 / seed.length);
}

/** The CIP-129 key-hash DRep id for `hash(seed)`. */
export function drepId(seed: string, isScript = false): string {
  return encodeCip129DRepId({ hash: hash(seed), isScript });
}

/** The CIP-129 action id for `txHash#index`. */
export function actionId(txHash: string, index: number): string {
  return encodeCip129GovActionId({ txHash, index });
}

/** A distinct CIP-129 DRep id per integer, for specs that need many. */
export function numberedDRepId(n: number): string {
  return encodeCip129DRepId({
    hash: n.toString(16).padStart(56, '0'),
    isScript: false,
  });
}
