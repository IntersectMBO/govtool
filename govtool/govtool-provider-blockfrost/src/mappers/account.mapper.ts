import type {
  Account,
  Delegation,
  DelegationTarget,
  StakeBalance,
} from '@govtool/data-providers/chain-data';

import {
  encodeCip105DRepId,
  encodeCip129DRepId,
  isScriptStakeAddress,
  stakeAddressToHash,
} from '../common/ids';
import type { BfAccount } from '../http/types';
import { bech32 } from 'bech32';

const BECH32_LIMIT = 1023;

/**
 * db-sync names the predefined options through `drep_hash.view`; Blockfrost
 * returns them as bech32 ids whose credential is all-zero for abstain and
 * all-one… in practice it returns the reserved ids below, which are the
 * CIP-129 encodings of the two ledger-defined options.
 */
function predefinedOption(
  drepId: string,
): 'alwaysAbstain' | 'alwaysNoConfidence' | null {
  try {
    const bytes = Buffer.from(
      bech32.fromWords(bech32.decode(drepId, BECH32_LIMIT).words),
    );
    // The predefined options are encoded with the reserved credential types
    // 0x02 (abstain) and 0x03 (no confidence) in CIP-129.
    if (bytes.length >= 1) {
      if (bytes[0] === 0x02) return 'alwaysAbstain';
      if (bytes[0] === 0x03) return 'alwaysNoConfidence';
    }
  } catch {
    /* fall through to the name check */
  }
  if (drepId.includes('always_abstain')) return 'alwaysAbstain';
  if (drepId.includes('always_no_confidence')) return 'alwaysNoConfidence';
  return null;
}

/**
 * A `drep_id` from Blockfrost → the contract's delegation target.
 *
 * Blockfrost is not consistent about which DRep encoding it returns:
 * `/governance/dreps` gives CIP-129 (29 bytes, credential-type header
 * included) while `/accounts/{stake}` gives CIP-105 (28 bytes, no header) for
 * the very same credential — verified on mainnet. Both are accepted here and
 * normalised, so `id` is always CIP-129 and `cip105Id` always the older form,
 * whichever endpoint the value came from.
 *
 * With CIP-105 the header is gone, so script-ness comes from the bech32
 * prefix (`drep_script1…`) instead.
 */
export function toDelegationTarget(drepId: string): DelegationTarget {
  const option = predefinedOption(drepId);
  if (option !== null) return { kind: 'predefined', option };

  let decoded: { prefix: string; bytes: Buffer } | null = null;
  try {
    const d = bech32.decode(drepId, BECH32_LIMIT);
    decoded = {
      prefix: d.prefix,
      bytes: Buffer.from(bech32.fromWords(d.words)),
    };
  } catch {
    decoded = null;
  }

  if (decoded === null) {
    // Not bech32 at all: report the id verbatim rather than dropping the
    // delegation, and leave the derived fields empty.
    return {
      kind: 'drep',
      drep: { role: 'drep', id: drepId, hash: '', isScriptBased: false },
    };
  }

  let hash: string;
  let isScript: boolean;
  if (decoded.bytes.length === 29) {
    isScript = decoded.bytes[0] === 0x23;
    hash = decoded.bytes.subarray(1).toString('hex');
  } else {
    isScript = decoded.prefix === 'drep_script';
    hash = decoded.bytes.toString('hex');
  }

  return {
    kind: 'drep',
    drep: {
      role: 'drep',
      id: encodeCip129DRepId(hash, isScript),
      hash,
      isScriptBased: isScript,
      cip105Id: encodeCip105DRepId(hash, isScript),
    },
  };
}

/**
 * Blockfrost's account record is the whole picture in one read: registration,
 * balance, the governance delegation and the pool delegation. The one thing
 * it does not carry is *which transaction* set the governance delegation, so
 * `txRef` is `null` — the field is nullable precisely for this.
 */
export function mapAccount(account: BfAccount): Account {
  const hash = stakeAddressToHash(account.stake_address);
  return {
    stakeAddress: account.stake_address,
    stakeKeyHash: hash ?? '',
    isRegistered: account.active,
    isScriptBased: isScriptStakeAddress(account.stake_address),
  };
}

export function mapBalance(account: BfAccount): StakeBalance {
  return {
    total: account.controlled_amount,
    rewards: account.rewards_sum,
    rewardsRest: account.reserves_sum,
  };
}

export function mapDelegation(account: BfAccount): Delegation | null {
  if (account.drep_id === null) return null;
  return { target: toDelegationTarget(account.drep_id), txRef: null };
}
