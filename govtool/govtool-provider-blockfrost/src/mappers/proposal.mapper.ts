import type {
  EpochStamp,
  GovAction,
  GovActionBody,
  GovActionLifecycle,
  GovActionRef,
  GovActionStatus,
  GovActionType,
  Ratio,
} from '@govtool/data-providers/chain-data';
import type {
  GovActionMetadataBody,
  MetadataProjection,
} from '@govtool/data-providers/metadata';

import { encodeCip129GovActionId } from '../common/ids';
import { asFiniteNumber, isRecord } from '../common/numbers';
import type {
  BfGovernanceDescription,
  BfProposal,
  BfProposalMetadata,
  BfProposalParameters,
  BfProposalWithdrawal,
} from '../http/types';
import { projectMetadata } from './metadata.mapper';

/**
 * Blockfrost reports the action type twice, and the two disagree:
 * `governance_type` is snake_case Blockfrost naming (`new_committee`) while
 * `governance_description.tag` is the ledger's own name (`UpdateCommittee`).
 * The contract uses ledger names, so the tag is preferred and this map is the
 * fallback for a record whose description is absent.
 */
const TYPE_BY_GOVERNANCE_TYPE: Record<string, GovActionType> = {
  info_action: 'InfoAction',
  no_confidence: 'NoConfidence',
  parameter_change: 'ParameterChange',
  hard_fork_initiation: 'HardForkInitiation',
  treasury_withdrawals: 'TreasuryWithdrawals',
  new_committee: 'UpdateCommittee',
  new_constitution: 'NewConstitution',
};

const LEDGER_TAGS: ReadonlySet<string> = new Set<GovActionType>([
  'InfoAction',
  'NoConfidence',
  'ParameterChange',
  'HardForkInitiation',
  'TreasuryWithdrawals',
  'UpdateCommittee',
  'NewConstitution',
]);

export function toContractType(proposal: BfProposal): GovActionType {
  const tag = proposal.governance_description?.tag;
  if (tag !== undefined && LEDGER_TAGS.has(tag)) {
    return tag as GovActionType;
  }
  return TYPE_BY_GOVERNANCE_TYPE[proposal.governance_type] ?? 'InfoAction';
}

/** The contract's type name → the `governance_type` Blockfrost filters on. */
export function toBlockfrostGovernanceType(type: GovActionType): string {
  for (const [key, value] of Object.entries(TYPE_BY_GOVERNANCE_TYPE)) {
    if (value === type) return key;
  }
  return 'info_action';
}

/**
 * Blockfrost carries every terminal epoch, so unlike a live-only source the
 * full status is derivable. Precedence follows the ledger: an action is
 * enacted only after being ratified, and dropped/expired are terminal.
 */
export function deriveStatus(proposal: BfProposal): GovActionStatus {
  if (proposal.enacted_epoch !== null) return 'enacted';
  if (proposal.ratified_epoch !== null) return 'ratified';
  if (proposal.dropped_epoch !== null) return 'dropped';
  if (proposal.expired_epoch !== null) return 'expired';
  return 'live';
}

export function govActionRef(txHash: string, index: number): GovActionRef {
  return { id: encodeCip129GovActionId(txHash, index), txHash, index };
}

/* ------------------------------------------------------------------------- */
/* Typed bodies                                                              */
/* ------------------------------------------------------------------------- */

function credentialOf(
  value: unknown,
): { hash: string; isScript: boolean } | null {
  if (!isRecord(value)) return null;
  const script = value.scriptHash;
  if (typeof script === 'string') return { hash: script, isScript: true };
  const key = value.keyHash;
  if (typeof key === 'string') return { hash: key, isScript: false };
  return null;
}

function toRatio(value: unknown): Ratio | null {
  if (!isRecord(value)) return null;
  const numerator = asFiniteNumber(value.numerator);
  const denominator = asFiniteNumber(value.denominator);
  return numerator === undefined || denominator === undefined
    ? null
    : { numerator, denominator };
}

/**
 * Builds the typed body from the ledger description, plus the two
 * sub-resources that carry what the description leaves encoded:
 * `/withdrawals` resolves treasury targets to bech32 stake addresses, and
 * `/parameters` gives the ParameterChange delta.
 *
 * Returns `undefined` where the shape is not what this version of Blockfrost
 * was verified to produce; the caller then reports `rawBody` only, rather
 * than a half-built body.
 */
export function buildBody(input: {
  type: GovActionType;
  description: BfGovernanceDescription | null;
  parameters: BfProposalParameters | null;
  withdrawals: BfProposalWithdrawal[] | null;
}): GovActionBody | undefined {
  const { type, description } = input;
  const contents = description?.contents;

  switch (type) {
    case 'InfoAction':
      return { type };
    case 'NoConfidence':
      return { type };

    case 'ParameterChange': {
      const changes = input.parameters?.parameters;
      if (!isRecord(changes)) return undefined;
      // Blockfrost returns every parameter with `null` for the ones this
      // action leaves alone; only the changed ones are the change set.
      const changed = Object.fromEntries(
        Object.entries(changes).filter(
          ([key, value]) => value !== null && key !== 'epoch',
        ),
      );
      const body: GovActionBody = { type, changes: changed };
      const script = Array.isArray(contents) ? contents[2] : undefined;
      if (typeof script === 'string') body.guardrailsScriptHash = script;
      return body;
    }

    case 'HardForkInitiation': {
      const version = Array.isArray(contents) ? contents[1] : undefined;
      if (!isRecord(version)) return undefined;
      const major = asFiniteNumber(version.major);
      const minor = asFiniteNumber(version.minor);
      if (major === undefined || minor === undefined) return undefined;
      return { type, protocolVersion: { major, minor } };
    }

    case 'TreasuryWithdrawals': {
      // The description encodes raw credentials; the sub-resource has them
      // as bech32 stake addresses, which is what the contract wants.
      if (input.withdrawals === null) return undefined;
      let total = 0n;
      const withdrawals = input.withdrawals.map((w) => {
        total += BigInt(w.amount);
        return { stakeAddress: w.stake_address, amount: w.amount };
      });
      const body: GovActionBody = {
        type,
        withdrawals,
        totalAmount: total.toString(),
      };
      const script = Array.isArray(contents) ? contents[1] : undefined;
      if (typeof script === 'string') body.guardrailsScriptHash = script;
      return body;
    }

    case 'NewConstitution': {
      const payload = Array.isArray(contents) ? contents[1] : undefined;
      if (!isRecord(payload) || !isRecord(payload.anchor)) return undefined;
      const { url, dataHash } = payload.anchor;
      if (typeof url !== 'string' || typeof dataHash !== 'string') {
        return undefined;
      }
      return {
        type,
        anchor: { url, dataHash },
        guardrailsScriptHash:
          typeof payload.script === 'string' ? payload.script : null,
      };
    }

    case 'UpdateCommittee': {
      // contents = [ null, [removed credentials], { "keyHash-<hex>": epoch },
      //              { numerator, denominator } ]
      if (!Array.isArray(contents) || contents.length < 4) return undefined;
      const quorum = toRatio(contents[3]);
      if (quorum === null) return undefined;

      const removed: { coldCredential: string; isScriptBased: boolean }[] = [];
      if (Array.isArray(contents[1])) {
        for (const entry of contents[1]) {
          const cred = credentialOf(entry);
          if (cred === null) return undefined;
          removed.push({
            coldCredential: cred.hash,
            isScriptBased: cred.isScript,
          });
        }
      }

      const added: {
        coldCredential: string;
        isScriptBased: boolean;
        termExpiryEpoch: number;
      }[] = [];
      if (isRecord(contents[2])) {
        for (const [key, value] of Object.entries(contents[2])) {
          const epoch = asFiniteNumber(value);
          if (epoch === undefined) return undefined;
          // Keys are `keyHash-<hex>` / `scriptHash-<hex>`.
          const isScript = key.startsWith('scriptHash-');
          const hash = key.replace(/^(keyHash|scriptHash)-/, '');
          added.push({
            coldCredential: hash,
            isScriptBased: isScript,
            termExpiryEpoch: epoch,
          });
        }
      }

      return { type, added, removed, quorum };
    }
  }
}

/* ------------------------------------------------------------------------- */
/* Whole entity                                                              */
/* ------------------------------------------------------------------------- */

export function proposalMetadata(
  metadata: BfProposalMetadata | null,
): MetadataProjection<GovActionMetadataBody> | null {
  if (metadata === null) return null;
  return projectMetadata<GovActionMetadataBody>({
    url: metadata.url,
    hash: metadata.hash,
    standard: 'CIP108',
    jsonMetadata: metadata.json_metadata,
  });
}

export function mapProposal(input: {
  proposal: BfProposal;
  metadata: BfProposalMetadata | null;
  parameters: BfProposalParameters | null;
  withdrawals: BfProposalWithdrawal[] | null;
  stamps: {
    expires: EpochStamp | null;
    ratifiedAt: EpochStamp | null;
    enactedAt: EpochStamp | null;
    droppedAt: EpochStamp | null;
    expiredAt: EpochStamp | null;
  };
}): GovAction {
  const { proposal } = input;
  const type = toContractType(proposal);

  const lifecycle: GovActionLifecycle = {
    status: deriveStatus(proposal),
    // Blockfrost's proposal record has no submission epoch or time, so
    // `submitted` is omitted — see the contract's note on the field. The
    // submitting transaction is the record's own key, so it is always known.
    submittedTx: { txHash: proposal.tx_hash, index: proposal.cert_index },
    expires: input.stamps.expires,
    ratifiedAt: input.stamps.ratifiedAt,
    enactedAt: input.stamps.enactedAt,
    droppedAt: input.stamps.droppedAt,
    expiredAt: input.stamps.expiredAt,
  };

  const action: GovAction = {
    ...govActionRef(proposal.tx_hash, proposal.cert_index),
    type,
    lifecycle,
    deposit: proposal.deposit,
    depositReturnAddress: proposal.return_address,
    // Who submitted it is not in the record; only where the deposit returns.
    previousAction: null,
    metadata: proposalMetadata(input.metadata),
  };

  if (proposal.governance_description !== null) {
    action.rawBody = proposal.governance_description;
  }
  const body = buildBody({
    type,
    description: proposal.governance_description,
    parameters: input.parameters,
    withdrawals: input.withdrawals,
  });
  if (body !== undefined) action.body = body;

  return action;
}
