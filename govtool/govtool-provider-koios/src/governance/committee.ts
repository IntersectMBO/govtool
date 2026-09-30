/**
 * `/governance/committee` over Koios (SPEC.md §5.5).
 *
 * Membership: `/committee_info` is the ledger's current committee — the
 * genesis committee with every enacted UpdateCommittee applied, dissolved by
 * an enacted NoConfidence, with each member's latest hot-key authorisation or
 * resignation and its expiry. That is the assembled ledger state the SPEC asks
 * for, so it is read, not rebuilt.
 *
 * Term start is not in `/committee_info`. It is derived from the enacted
 * committee lineage (`/proposal_list` bodies, ordered by predecessor): the
 * seat started at the earliest step of the unbroken run of steps, ending now,
 * that include the member, as the db-sync provider defines it. Koios does not
 * serve the genesis committee's membership. A member never added by any
 * UpdateCommittee must be a genesis member; one whose current hot key voted
 * before the first UpdateCommittee took effect was on the genesis committee
 * too. Otherwise, when the run could reach back past the first enacted
 * UpdateCommittee — the member was added by one but may also have sat on the
 * genesis committee — the start is unknown and `null`.
 *
 * Constitution: derived, per SPEC.md §5.5 — the enacted head of the
 * constitution lineage carries the anchor and guardrails script.
 */
import type {
  Committee,
  CommitteeApi,
  CommitteeMember,
  Constitution,
  GovActionRef,
  Ratio,
} from '@govtool/data-providers/chain-data';

import type { Ctx } from '../context';
import { internal, invalidInput, notFound, unsupported } from '../errors';
import { decodeCommitteeColdId, encodeCommitteeColdId, encodeCommitteeHotId } from '../ids';
import { requireRatio } from '../ratio';
import type { CommitteeInfoRow, ProposalRow } from '../rows';
import { orderLineage } from './lineage';
import { decodeBody } from './proposals/body';
import { PROPOSAL_SELECT, refOf } from './proposals/rows';

export async function readCommitteeInfo(ctx: Ctx): Promise<CommitteeInfoRow | undefined> {
  const { rows } = await ctx.http.get<CommitteeInfoRow>('committee_info');
  return rows[0];
}

/** Hot credential -> cold credential, for the authorisations in force now. */
export function coldByHot(info: CommitteeInfoRow | undefined): Map<string, string> {
  const out = new Map<string, string>();
  for (const m of info?.members ?? []) {
    if (!m.cc_hot_hex || m.cc_hot_has_script === null) continue;
    out.set(
      encodeCommitteeHotId(m.cc_hot_hex.toLowerCase(), m.cc_hot_has_script),
      encodeCommitteeColdId(m.cc_cold_hex.toLowerCase(), m.cc_cold_has_script),
    );
  }
  return out;
}

/** Every enacted action of the given Koios types, oldest first along the predecessor chain. */
export async function enactedLineage(ctx: Ctx, lineage: string, koiosTypes: readonly string[]): Promise<ProposalRow[]> {
  const rows = await ctx.http.getAll<ProposalRow>(
    'proposal_list',
    { proposal_type: `in.(${koiosTypes.join(',')})`, enacted_epoch: 'not.is.null' },
    { select: PROPOSAL_SELECT, order: 'block_time.asc,proposal_index.asc' },
  );
  return orderLineage(
    lineage,
    rows.map((row) => ({ ...row, id: row.proposal_id, prevId: row.previous_gov_action_proposal_id })),
  );
}

interface Step {
  ref: GovActionRef;
  epoch: number;
  dissolves: boolean;
  added: Set<string>;
  removed: Set<string>;
  quorum: Ratio | null;
}

type Presence = 'in' | 'out' | 'unknown';

/**
 * Term start per SPEC/db-sync semantics, or null where Koios cannot know it.
 * `conwayStart` is when the genesis committee's terms began.
 */
export function termStart(member: string, steps: readonly Step[], conwayStart: number | null, genesisMember = false): number | null {
  // Presence before each step, walking forward from genesis. A member never
  // added by any step can only be on the committee from genesis.
  const everAdded = steps.some((s) => s.added.has(member));
  let presence: Presence = !everAdded || genesisMember ? 'in' : 'unknown';
  const before: Presence[] = [];
  for (const step of steps) {
    before.push(presence);
    if (step.dissolves) presence = 'out';
    else if (step.added.has(member)) presence = 'in';
    else if (step.removed.has(member)) presence = 'out';
  }
  if (presence !== 'in') return null;
  // Walk back from the head while the member was already seated before the step.
  for (let k = steps.length - 1; k >= 0; k--) {
    const b = before[k]!;
    if (b === 'out') return steps[k]!.epoch;
    if (b === 'unknown') return null;
  }
  return conwayStart;
}

function toSteps(rows: readonly ProposalRow[], ctx: Ctx): Step[] {
  return rows.map((row) => {
    const ref = refOf(row.proposal_id, row.proposal_tx_hash, row.proposal_index);
    const epoch = row.enacted_epoch!;
    if (row.proposal_type === 'NoConfidence') {
      return { ref, epoch, dissolves: true, added: new Set(), removed: new Set(), quorum: null };
    }
    const { body } = decodeBody(row.proposal_type, row.proposal_description, ctx.network);
    if (body.type !== 'UpdateCommittee') throw internal('committee lineage holds a non-committee action', { id: ref.id });
    return {
      ref,
      epoch,
      dissolves: false,
      added: new Set(body.added.map((a) => a.coldCredential)),
      removed: new Set(body.removed.map((r) => r.coldCredential)),
      quorum: body.quorum,
    };
  });
}

export async function readCommittee(ctx: Ctx): Promise<Committee> {
  const [info, lineageRows, conway] = await Promise.all([
    readCommitteeInfo(ctx),
    enactedLineage(ctx, 'committee', ['NewCommittee', 'NoConfidence']),
    ctx.http.get<{ epoch_no: number }>('epoch_params', { protocol_major: 'gte.9' }, { select: 'epoch_no', order: 'epoch_no.asc', limit: 1 }),
  ]);
  if (!info) throw internal('Koios returned no committee');
  const steps = toSteps(lineageRows, ctx);
  const head = steps[steps.length - 1];
  const headId = info.proposal_id === null ? null : refOf(info.proposal_id).id;
  if ((head?.ref.id ?? null) !== headId) {
    throw internal('Koios committee_info and the enacted committee lineage disagree on the head', { committeeInfo: headId });
  }
  const conwayStart = conway.rows[0]?.epoch_no ?? null;

  // Hot keys that voted while only the genesis committee existed.
  const first = steps[0];
  const genesisVoters = first
    ? new Set(
        (
          await ctx.http.getAll<{ voter_id: string }>(
            'vote_list',
            { voter_role: 'eq.ConstitutionalCommittee', epoch_no: `lt.${first.epoch}` },
            { select: 'voter_id' },
          )
        ).map((v) => v.voter_id),
      )
    : new Set<string>();

  let quorum: Ratio | undefined;
  if (info.quorum_numerator !== null && info.quorum_denominator !== null) {
    quorum = requireRatio({ numerator: info.quorum_numerator, denominator: info.quorum_denominator }, 'committee quorum');
  } else {
    // Dissolved: the quorum reported is the last one the ledger held.
    quorum = [...steps].reverse().find((s) => s.quorum !== null)?.quorum ?? undefined;
  }
  if (!quorum) throw unsupported('the committee quorum', 'the committee is dissolved and Koios does not serve the genesis quorum');

  const members = (info.members ?? []).map((m): CommitteeMember => {
    const cold = encodeCommitteeColdId(m.cc_cold_hex.toLowerCase(), m.cc_cold_has_script);
    const resigned = m.status === 'resigned';
    const hot =
      m.status === 'authorized' && m.cc_hot_hex && m.cc_hot_has_script !== null
        ? encodeCommitteeHotId(m.cc_hot_hex.toLowerCase(), m.cc_hot_has_script)
        : null;
    return {
      role: 'cc',
      coldCredential: cold,
      hotCredential: hot,
      termStartEpoch: termStart(cold, steps, conwayStart, hot !== null && genesisVoters.has(hot)),
      termExpiryEpoch: m.expiration_epoch ?? null,
      hasResigned: resigned,
      isScriptBased: m.cc_cold_has_script,
    };
  });
  members.sort((a, b) => (a.coldCredential < b.coldCredential ? -1 : a.coldCredential > b.coldCredential ? 1 : 0));
  return { members, quorum, enactedBy: head?.ref ?? null, isDissolved: head?.dissolves ?? false };
}

export async function readConstitution(ctx: Ctx): Promise<Constitution> {
  const rows = await enactedLineage(ctx, 'constitution', ['NewConstitution']);
  const head = rows[rows.length - 1];
  if (!head) {
    throw unsupported('the genesis constitution', 'no NewConstitution has been enacted and Koios does not serve the genesis one');
  }
  const { body } = decodeBody(head.proposal_type, head.proposal_description, ctx.network);
  if (body.type !== 'NewConstitution') throw internal('constitution lineage holds a non-constitution action');
  const epoch = head.enacted_epoch!;
  return {
    anchor: body.anchor,
    guardrailsScriptHash: body.guardrailsScriptHash ?? null,
    enactedBy: refOf(head.proposal_id, head.proposal_tx_hash, head.proposal_index),
    enactedAt: { epoch, time: await ctx.chain.epochStart(epoch) },
  };
}

export function createCommitteeApi(ctx: Ctx): CommitteeApi {
  return {
    getCommittee: async () => ctx.envelope(await readCommittee(ctx)),

    getMember: async (coldCredential) => {
      if (typeof coldCredential !== 'string') throw invalidInput('coldCredential must be a CIP-129 cc_cold id');
      const credential = decodeCommitteeColdId(coldCredential.trim());
      const id = encodeCommitteeColdId(credential.hash, credential.isScript);
      const committee = await readCommittee(ctx);
      const member = committee.members.find((m) => m.coldCredential === id);
      if (!member) throw notFound('Not a member of the current committee', { coldCredential: id });
      return ctx.envelope(member);
    },

    getConstitution: async () => ctx.envelope(await readConstitution(ctx)),
  };
}
