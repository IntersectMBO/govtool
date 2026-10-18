/**
 * `/governance/committee` (SPEC.md §5.5).
 *
 * Hosted Blockfrost serves the committee as LEDGER STATE — the membership in
 * force, the quorum, whether it is dissolved and the action that set it — so
 * nothing is replayed from actions here. (The self-hosted blockfrost-ryo 3.1.1
 * had no such resource.)
 *
 *   termStartEpoch  null: the resource gives each member's expiry, not when
 *                   the seat began, and a replay of UpdateCommittee deltas
 *                   would be a reconstruction.
 *   constitution    `/governance/constitution` answers 400 (no such path); it
 *                   is derived as SPEC.md §5.5 allows — the enacted head of the
 *                   constitution lineage, whose body carries the anchor.
 */
import type { CommitteeApi, Committee, CommitteeMember, Constitution } from '@govtool/data-providers/chain-data';

import type { Ctx, Session } from '../context';
import { internal, notFound, unsupported } from '../errors';
import {
  decodeCommitteeColdId,
  encodeCommitteeColdId,
  encodeCommitteeHotId,
  encodeGovActionId,
  isHex,
  type Credential,
} from '../ids';
import { enactedHead } from './proposals/enacted';
import { decodeRecord, requireRecord } from './proposals/records';

export interface BfCommitteeMember {
  cc_cold_id: string;
  cc_cold_hex: string;
  cc_cold_has_script: boolean;
  cc_hot_id: string | null;
  cc_hot_hex: string | null;
  cc_hot_has_script: boolean | null;
  /** `authorized` on mainnet; `not_authorized` and `resigned` otherwise. */
  status: string;
  expiration_epoch: number | null;
}

export interface BfCommittee {
  gov_action_id: string | null;
  proposal_tx_hash: string | null;
  proposal_index: number | null;
  is_dissolved: boolean;
  quorum: { numerator: number; denominator: number } | null;
  members: BfCommitteeMember[];
}

export interface CommitteeState {
  committee: Committee;
  /** Hot credential key (`k:`/`s:` + hash) → the member it currently votes for. */
  byHot: Map<string, CommitteeMember>;
  /** Bare hot hash → its credential, for resolving a hex voter. */
  hotCredentials: Map<string, Credential>;
}

export const credentialKey = (c: Credential) => `${c.isScript ? 's' : 'k'}:${c.hash}`;

const KNOWN_STATUS = new Set(['authorized', 'not_authorized', 'resigned']);

export function mapCommittee(raw: BfCommittee): CommitteeState {
  if (!raw.quorum || !Number.isSafeInteger(raw.quorum.numerator) || !Number.isSafeInteger(raw.quorum.denominator) || raw.quorum.denominator <= 0) {
    throw internal('Blockfrost reports a committee without a valid quorum');
  }
  const byHot = new Map<string, CommitteeMember>();
  const hotCredentials = new Map<string, Credential>();
  const members = raw.members.map((m): CommitteeMember => {
    if (!isHex(m.cc_cold_hex, 28)) throw internal('Blockfrost committee member has no cold credential');
    const cold = { hash: m.cc_cold_hex.toLowerCase(), isScript: m.cc_cold_has_script };
    // The id Blockfrost sends must be the CIP-129 form of the hex it sends.
    const decoded = decodeCommitteeColdId(m.cc_cold_id);
    if (decoded.hash !== cold.hash || decoded.isScript !== cold.isScript) {
      throw internal('Blockfrost committee cold id disagrees with its hex', { coldCredential: m.cc_cold_id });
    }
    if (!KNOWN_STATUS.has(m.status)) throw internal(`Blockfrost reports an unknown committee member status '${m.status}'`);
    const resigned = m.status === 'resigned';
    const hot =
      m.status === 'authorized' && m.cc_hot_hex && isHex(m.cc_hot_hex, 28) && typeof m.cc_hot_has_script === 'boolean'
        ? { hash: m.cc_hot_hex.toLowerCase(), isScript: m.cc_hot_has_script }
        : null;
    const member: CommitteeMember = {
      role: 'cc',
      coldCredential: encodeCommitteeColdId(cold.hash, cold.isScript),
      hotCredential: hot ? encodeCommitteeHotId(hot.hash, hot.isScript) : null,
      termStartEpoch: null,
      termExpiryEpoch: m.expiration_epoch,
      hasResigned: resigned,
      isScriptBased: cold.isScript,
    };
    if (hot) {
      byHot.set(credentialKey(hot), member);
      hotCredentials.set(hot.hash, hot);
    }
    return member;
  });
  members.sort((a, b) => (a.coldCredential < b.coldCredential ? -1 : a.coldCredential > b.coldCredential ? 1 : 0));

  let enactedBy: Committee['enactedBy'] = null;
  if (raw.proposal_tx_hash && raw.proposal_index !== null) {
    const id = encodeGovActionId(raw.proposal_tx_hash, raw.proposal_index);
    if (raw.gov_action_id && raw.gov_action_id !== id) throw internal('Blockfrost committee action id disagrees with its transaction');
    enactedBy = { id, txHash: raw.proposal_tx_hash, index: raw.proposal_index };
  }
  return {
    committee: {
      members,
      quorum: { numerator: raw.quorum.numerator, denominator: raw.quorum.denominator },
      enactedBy,
      isDissolved: raw.is_dissolved,
    },
    byHot,
    hotCredentials,
  };
}

export const loadCommittee = (s: Session): Promise<CommitteeState> =>
  s.once('committee', async () => mapCommittee(await s.http.get<BfCommittee>('/governance/committee')));

export async function readConstitution(ctx: Ctx, s: Session): Promise<Constitution> {
  const head = await enactedHead(s, 'constitution', ctx.network);
  if (!head) {
    // The genesis constitution is not a proposal, and Blockfrost has no
    // constitution resource to read it from.
    throw unsupported('the genesis constitution (no NewConstitution has been enacted)');
  }
  const record = await requireRecord(s, head);
  const { body } = decodeRecord(record, ctx.network);
  if (body.type !== 'NewConstitution') throw internal('Enacted constitution-lineage action is not a NewConstitution');
  return {
    anchor: body.anchor,
    guardrailsScriptHash: body.guardrailsScriptHash ?? null,
    enactedBy: head,
    enactedAt: record.enacted === null ? null : { epoch: record.enacted },
  };
}

export function createCommitteeApi(ctx: Ctx): CommitteeApi {
  return {
    getCommittee: async () => ctx.envelope((await loadCommittee(ctx.session())).committee),

    getMember: async (coldCredential) => {
      const credential = decodeCommitteeColdId(coldCredential);
      const id = encodeCommitteeColdId(credential.hash, credential.isScript);
      const { committee } = await loadCommittee(ctx.session());
      const member = committee.members.find((m) => m.coldCredential === id);
      if (!member) throw notFound('Not a member of the current committee', { coldCredential });
      return ctx.envelope(member);
    },

    getConstitution: async () => ctx.envelope(await readConstitution(ctx, ctx.session())),
  };
}
