import { bech32 } from 'bech32';
import type { Committee, GovAction } from '@govtool/data-providers/chain-data';

import { dbInteger, type ApiInteger } from './integer';

export type LegacyDescription =
  Record<string, unknown> | { receivingAddress: string; amount: ApiInteger }[];

/** CIP-129 committee cold credential → its hash and key/script flag. */
export function decodeColdCredential(
  id: string,
): { hash: string; isScript: boolean } | null {
  try {
    const decoded = bech32.decode(id, 1023);
    const bytes = Buffer.from(bech32.fromWords(decoded.words));
    if (decoded.prefix !== 'cc_cold' || bytes.length !== 29) return null;
    if (bytes[0] !== 0x12 && bytes[0] !== 0x13) return null;
    return {
      hash: bytes.subarray(1).toString('hex'),
      isScript: bytes[0] === 0x13,
    };
  } catch {
    return null;
  }
}

const ledgerRef = (action: GovAction) =>
  action.previousAction === null
    ? null
    : {
        govActionIx: action.previousAction.index,
        txId: action.previousAction.txHash,
      };

/**
 * db-sync's `description` column as the Haskell backend reshaped it by type,
 * in the shapes the frontend's detail tabs read on both the `/proposal`
 * (`details`) and outcomes (`description`) routes: withdrawals as an array, the hard fork version, the constitution
 * anchor and guardrails script, and the committee change with each added
 * member's current and new term. `{}` where the action proposes nothing
 * the UI renders.
 */
export function toLegacyDescription(
  action: GovAction,
  committee: Committee | null,
): LegacyDescription {
  const body = action.body;
  switch (body.type) {
    case 'TreasuryWithdrawals':
      return body.withdrawals.map((w) => ({
        receivingAddress: w.stakeAddress,
        amount: dbInteger(w.amount),
      }));
    case 'InfoAction':
      return { data: { tag: 'InfoAction' } };
    case 'HardForkInitiation':
      return {
        major: body.protocolVersion.major,
        minor: body.protocolVersion.minor,
      };
    case 'NoConfidence':
      return { data: ledgerRef(action) };
    case 'ParameterChange':
      // The UI renders a parameter change from `proposal_params`; this is the
      // ledger's [previous action, changes, guardrails] triple, with the
      // contract's parameter names.
      return {
        data: [
          ledgerRef(action),
          body.changes,
          body.guardrailsScriptHash ?? null,
        ],
      };
    case 'NewConstitution':
      return {
        anchor: { dataHash: body.anchor.dataHash, url: body.anchor.url },
        script: body.guardrailsScriptHash ?? null,
      };
    case 'UpdateCommittee': {
      const current = new Map(
        (committee?.members ?? []).map((m) => [
          m.coldCredential,
          m.termExpiryEpoch,
        ]),
      );
      const credential = (id: string) => {
        const decoded = decodeColdCredential(id);
        return {
          hash: decoded?.hash ?? id,
          type: decoded?.isScript ? 'scriptHash' : 'keyHash',
          hasScript: decoded?.isScript ?? false,
        };
      };
      return {
        tag: 'UpdateCommittee',
        members: body.added.map((m) => {
          const c = credential(m.coldCredential);
          return {
            hash: c.hash,
            type: c.type,
            expirationEpoch: current.get(m.coldCredential) ?? null,
            hasScript: c.hasScript,
            newExpirationEpoch: m.termExpiryEpoch,
          };
        }),
        membersToBeRemoved: body.removed.map((m) =>
          credential(m.coldCredential),
        ),
        threshold:
          body.quorum.denominator === 0
            ? null
            : body.quorum.numerator / body.quorum.denominator,
      };
    }
  }
}
