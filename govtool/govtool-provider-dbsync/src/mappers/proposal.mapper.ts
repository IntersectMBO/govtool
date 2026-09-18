import type {
  EnactedActionSummary,
  GovAction,
  GovActionBody,
  GovActionRef,
  GovActionType,
  RoleTally,
} from '@govtool/data-providers/chain-data';
import type {
  GovActionMetadataBody,
  MetadataAuthor,
  MetadataProjection,
} from '@govtool/data-providers/metadata';

import { encodeCip129GovActionId } from '../common/ids';
import {
  toInteger,
  toIsoString,
  toLovelace,
  toNullableInteger,
} from '../common/numbers';
import type { EnactedProposalDetailsRow, ProposalRow } from '../rows';
import { definedFields, projectMetadata } from './metadata.mapper';

/* ------------------------------------------------------------------------- */
/* Type names: db-sync ↔ contract                                             */
/* ------------------------------------------------------------------------- */

/**
 * db-sync (and the legacy API) call the committee action `NewCommittee`; the
 * ledger and the contract call it `UpdateCommittee`. Anything unknown becomes
 * `InfoAction`, exactly as the legacy backend did.
 */
const DB_TO_CONTRACT_TYPE: Record<string, GovActionType> = {
  ParameterChange: 'ParameterChange',
  HardForkInitiation: 'HardForkInitiation',
  TreasuryWithdrawals: 'TreasuryWithdrawals',
  NoConfidence: 'NoConfidence',
  NewCommittee: 'UpdateCommittee',
  NewConstitution: 'NewConstitution',
  InfoAction: 'InfoAction',
};

export function toContractType(dbType: string): GovActionType {
  return DB_TO_CONTRACT_TYPE[dbType] ?? 'InfoAction';
}

export function toDbSyncType(type: GovActionType): string {
  return type === 'UpdateCommittee' ? 'NewCommittee' : type;
}

/* ------------------------------------------------------------------------- */
/* Body: typed where the shaped description allows, raw always               */
/* ------------------------------------------------------------------------- */

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null && !Array.isArray(value);
}

/**
 * `list-proposals.sql` pre-shapes `description` per type. The variants whose
 * shape is unambiguous are typed here; `UpdateCommittee` is not, because the
 * SQL renders its threshold as a float and the contract wants the ratio.
 */
export function buildBody(
  type: GovActionType,
  description: unknown,
  proposalParams: unknown,
): GovActionBody | undefined {
  switch (type) {
    case 'InfoAction':
      return { type };
    case 'NoConfidence':
      return { type };
    case 'ParameterChange':
      return isRecord(proposalParams)
        ? { type, changes: proposalParams }
        : undefined;
    case 'HardForkInitiation': {
      if (!isRecord(description)) return undefined;
      const { major, minor } = description;
      return typeof major === 'number' && typeof minor === 'number'
        ? { type, protocolVersion: { major, minor } }
        : undefined;
    }
    case 'TreasuryWithdrawals': {
      if (!Array.isArray(description)) return undefined;
      const withdrawals: { stakeAddress: string; amount: string }[] = [];
      let total = 0n;
      for (const entry of description) {
        if (!isRecord(entry)) return undefined;
        const { receivingAddress, amount } = entry;
        if (
          typeof receivingAddress !== 'string' ||
          (typeof amount !== 'number' && typeof amount !== 'string')
        ) {
          return undefined;
        }
        const lovelace = toLovelace(amount);
        try {
          total += BigInt(lovelace);
        } catch {
          return undefined;
        }
        withdrawals.push({ stakeAddress: receivingAddress, amount: lovelace });
      }
      return { type, withdrawals, totalAmount: total.toString() };
    }
    case 'NewConstitution': {
      if (!isRecord(description) || !isRecord(description.anchor)) {
        return undefined;
      }
      const { url, dataHash } = description.anchor;
      if (typeof url !== 'string' || typeof dataHash !== 'string') {
        return undefined;
      }
      const script = description.script;
      return {
        type,
        anchor: { url, dataHash },
        guardrailsScriptHash: typeof script === 'string' ? script : null,
      };
    }
    case 'UpdateCommittee':
      return undefined;
  }
}

/* ------------------------------------------------------------------------- */
/* Row → entity                                                               */
/* ------------------------------------------------------------------------- */

function govActionRef(
  txHash: string,
  index: number,
  providerId?: string,
): GovActionRef {
  const ref: GovActionRef = {
    id: encodeCip129GovActionId(txHash, index),
    txHash,
    index,
  };
  if (providerId !== undefined) {
    ref.providerId = providerId;
  }
  return ref;
}

function proposalMetadata(
  row: ProposalRow,
): MetadataProjection<GovActionMetadataBody> | null {
  if (row.url === null) {
    return null;
  }
  const { body, hasData } = definedFields<
    Omit<GovActionMetadataBody, 'authors' | 'references'>
  >({
    title: row.title,
    abstract: row.abstract,
    motivation: row.motivation,
    rationale: row.rationale,
  });
  const fullBody: GovActionMetadataBody = { ...body };
  if (row.authors !== null && row.authors !== undefined) {
    fullBody.authors = row.authors as MetadataAuthor[];
  }
  return projectMetadata({
    anchor: { url: row.url, dataHash: row.data_hash ?? '' },
    standard: 'CIP108',
    body: fullBody,
    hasBodyData: hasData || row.json_content !== null,
    raw: row.json_content,
  });
}

/** Tallies as the ledger counts them: DReps and SPOs by stake, the committee by head. */
function tallies(row: ProposalRow): RoleTally[] {
  return [
    {
      role: 'drep',
      stake: {
        yes: toLovelace(row.yes_votes),
        no: toLovelace(row.no_votes),
        abstain: toLovelace(row.abstain_votes),
      },
    },
    {
      role: 'spo',
      stake: {
        yes: toLovelace(row.pool_yes_votes),
        no: toLovelace(row.pool_no_votes),
        abstain: toLovelace(row.pool_abstain_votes),
      },
    },
    {
      role: 'cc',
      count: {
        yes: toInteger(row.cc_yes_votes),
        no: toInteger(row.cc_no_votes),
        abstain: toInteger(row.cc_abstain_votes),
      },
    },
  ];
}

/** One `list-proposals.sql` row → `GovAction`. The statement returns live actions only. */
export function mapProposalRow(row: ProposalRow): GovAction {
  const index = toInteger(row.index);
  const type = toContractType(row.type);

  let previousAction: GovActionRef | null = null;
  if (
    row.prev_gov_action_tx_hash !== null &&
    row.prev_gov_action_index !== null
  ) {
    previousAction = govActionRef(
      row.prev_gov_action_tx_hash,
      toInteger(row.prev_gov_action_index),
    );
  }

  const expiryEpoch = toNullableInteger(row.expiration);
  let expires: GovAction['lifecycle']['expires'] = null;
  if (row.expiry_date !== null) {
    expires = { time: toIsoString(row.expiry_date) };
    if (expiryEpoch !== null) {
      expires.epoch = expiryEpoch;
    }
  }

  const action: GovAction = {
    ...govActionRef(row.tx_hash, index, String(row.id)),
    type,
    rawBody: row.description,
    lifecycle: {
      status: 'live',
      submitted: {
        epoch: toInteger(row.epoch_no),
        time: toIsoString(row.time),
      },
      submittedTx: { txHash: row.tx_hash, index },
      expires,
      ratifiedAt: null,
      enactedAt: null,
      droppedAt: null,
      expiredAt: null,
    },
    previousAction,
    metadata: proposalMetadata(row),
    tallies: tallies(row),
  };

  const body = buildBody(type, row.description, row.proposal_params);
  if (body !== undefined) {
    action.body = body;
  }

  return action;
}

/** The enacted-details row → summary. `description` here is db-sync's raw JSON, not the shaped one. */
export function mapEnactedRow(
  type: GovActionType,
  row: EnactedProposalDetailsRow,
): EnactedActionSummary {
  const index = toInteger(row.index);
  const summary: EnactedActionSummary = {
    type,
    action: govActionRef(row.hash, index, String(row.id)),
    submittedTx: { txHash: row.hash, providerId: String(row.tx_id) },
  };
  if (row.description !== null && row.description !== undefined) {
    summary.rawBody = row.description;
  }
  return summary;
}

/** `txHash#index`, the key both the vote statement and the legacy API use. */
export function legacyIdOf(action: GovActionRef): string {
  return `${action.txHash}#${action.index}`;
}
