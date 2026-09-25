/**
 * The current constitutional committee, assembled per SPEC.md §5.5.
 *
 * Membership is ledger state built from deltas, so it is never read off the
 * latest action. The inputs, and where each comes from in db-sync 13.x:
 *
 *   genesis committee        `committee` row with no `gov_action_proposal_id`
 *   enacted UpdateCommittee  `gov_action_proposal` (type NewCommittee) with an
 *                            `enacted_epoch`, and the `committee` row db-sync
 *                            writes for it, which holds the membership that
 *                            results from applying the action's add/remove
 *                            delta and its quorum
 *   enacted NoConfidence     `gov_action_proposal` (type NoConfidence) with an
 *                            `enacted_epoch`: the committee is dissolved
 *   AuthCommitteeHotCert     `committee_registration`
 *   ResignCommitteeColdCert  `committee_de_registration`
 *   term expiry              `committee_member.expiration_epoch`
 *
 * The enacted committee-lineage actions are ordered by their predecessor
 * links, and the head decides the membership. `epoch_state.committee_id` is
 * deliberately not the source: it is the same fact read another way, and the
 * live conformance script checks the two agree along with a full replay of the
 * add/remove deltas from genesis.
 *
 * A credential is its hash AND its key/script flag: a key hash and a script
 * hash with the same bytes are different members, which is why every lookup
 * here goes through `committee_hash.id` rather than the raw hash.
 */
import type { Committee, CommitteeMember, GovActionRef, Ratio } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../../context';
import { internal } from '../../errors';
import { encodeCommitteeColdId, encodeCommitteeHotId, encodeGovActionId } from '../../ids';
import { toInt, toNullableInt } from '../../numbers';
import { orderLineage } from './lineage';

interface MemberJson {
  hashId: string;
  hash: string;
  isScript: boolean;
  expiry: number | null;
}

export interface CommitteeStepRow {
  /** Null for the genesis committee row. */
  id: string | null;
  type: 'NewCommittee' | 'NoConfidence' | null;
  enacted_epoch: number | null;
  prev_id: string | null;
  tx_hash: string | null;
  index: number | null;
  committee_id: string | null;
  quorum_numerator: string | null;
  quorum_denominator: string | null;
  members: MemberJson[] | null;
}

export interface CertRow {
  cold_key_id: string;
  resigned: boolean;
  hot_hash: string | null;
  hot_is_script: boolean | null;
}

/**
 * Every enacted committee-lineage action with the membership db-sync recorded
 * for it, plus the genesis committee. The lineage has a handful of rows on any
 * network, so the aggregate per row is cheap.
 */
export const COMMITTEE_STEPS_SQL = `
  WITH steps AS (
    SELECT g.id::text AS id, g.type::text AS type, g.enacted_epoch::int AS enacted_epoch,
           g.prev_gov_action_proposal::text AS prev_id, encode(t.hash, 'hex') AS tx_hash, g.index::int AS index,
           c.id AS committee_pk
      FROM gov_action_proposal g
      JOIN tx t ON t.id = g.tx_id
      LEFT JOIN committee c ON c.gov_action_proposal_id = g.id
     WHERE g.type IN ('NewCommittee', 'NoConfidence') AND g.enacted_epoch IS NOT NULL
    UNION ALL
    SELECT NULL, NULL, NULL, NULL, NULL, NULL, c.id
      FROM committee c
     WHERE c.gov_action_proposal_id IS NULL
  )
  SELECT s.id, s.type, s.enacted_epoch, s.prev_id, s.tx_hash, s.index,
         c.id::text AS committee_id, c.quorum_numerator::text AS quorum_numerator,
         c.quorum_denominator::text AS quorum_denominator,
         (SELECT json_agg(json_build_object(
                   'hashId', ch.id::text, 'hash', encode(ch.raw, 'hex'),
                   'isScript', ch.has_script, 'expiry', cm.expiration_epoch))
            FROM committee_member cm
            JOIN committee_hash ch ON ch.id = cm.committee_hash_id
           WHERE cm.committee_id = c.id) AS members
    FROM steps s
    LEFT JOIN committee c ON c.id = s.committee_pk`;

/**
 * The latest hot-key certificate per cold credential: an authorisation names
 * the hot credential, a resignation ends the seat's ability to vote. The two
 * tables are small on every network (one row per certificate ever issued).
 */
export const COMMITTEE_CERTS_SQL = `
  SELECT DISTINCT ON (x.cold_key_id) x.cold_key_id::text AS cold_key_id, x.resigned,
         encode(h.raw, 'hex') AS hot_hash, h.has_script AS hot_is_script
    FROM (SELECT cold_key_id, hot_key_id, tx_id, cert_index, false AS resigned FROM committee_registration
          UNION ALL
          SELECT cold_key_id, NULL::bigint, tx_id, cert_index, true FROM committee_de_registration) x
    LEFT JOIN committee_hash h ON h.id = x.hot_key_id
   ORDER BY x.cold_key_id, x.tx_id DESC, x.cert_index DESC`;

/** The first Conway epoch, where the genesis committee's terms begin. */
export const CONWAY_START_SQL = `SELECT min(epoch_no)::int AS epoch FROM epoch_param WHERE protocol_major >= 9`;

interface Step {
  /** Null for genesis. */
  ref: GovActionRef | null;
  dissolves: boolean;
  /** Epoch the step took effect; for genesis, the first Conway epoch if known. */
  epoch: number | null;
  quorum: Ratio | null;
  members: MemberJson[];
}

const memberKey = (m: { hash: string; isScript: boolean }) => `${m.isScript ? 's' : 'k'}:${m.hash}`;

function toRatio(numerator: string | null, denominator: string | null): Ratio | null {
  if (numerator === null || denominator === null) return null;
  const n = toInt(numerator);
  const d = toInt(denominator);
  if (d === 0) throw internal('db-sync records a committee quorum with a zero denominator');
  return { numerator: n, denominator: d };
}

function toStep(row: CommitteeStepRow, conwayStart: number | null): Step {
  if (row.id === null) {
    return {
      ref: null,
      dissolves: false,
      epoch: conwayStart,
      quorum: toRatio(row.quorum_numerator, row.quorum_denominator),
      members: row.members ?? [],
    };
  }
  const ref: GovActionRef = {
    id: encodeGovActionId(row.tx_hash!, row.index!),
    txHash: row.tx_hash!,
    index: row.index!,
  };
  if (row.type === 'NoConfidence') {
    return { ref, dissolves: true, epoch: row.enacted_epoch, quorum: null, members: [] };
  }
  if (row.committee_id === null) {
    throw internal('db-sync has no committee row for an enacted UpdateCommittee action', { id: ref.id });
  }
  return {
    ref,
    dissolves: false,
    epoch: row.enacted_epoch,
    quorum: toRatio(row.quorum_numerator, row.quorum_denominator),
    members: row.members ?? [],
  };
}

/** Pure assembly, exported for tests: the rows of the three queries in, the committee out. */
export function assembleCommittee(
  stepRows: readonly CommitteeStepRow[],
  certRows: readonly CertRow[],
  conwayStart: number | null,
): Committee {
  const genesisRows = stepRows.filter((row) => row.id === null);
  if (genesisRows.length !== 1) {
    throw internal(`db-sync records ${genesisRows.length} genesis committees; expected exactly one`);
  }
  const enacted = orderLineage(
    'committee',
    stepRows.filter((row): row is CommitteeStepRow & { id: string } => row.id !== null).map((row) => ({ ...row, prevId: row.prev_id })),
  );
  const steps = [toStep(genesisRows[0]!, conwayStart), ...enacted.map((row) => toStep(row, conwayStart))];
  const head = steps[steps.length - 1]!;

  // A dissolved committee has no quorum of its own; the one reported is that
  // of the committee NoConfidence removed, which is the last one the ledger held.
  const quorum = [...steps].reverse().find((step) => step.quorum !== null)?.quorum;
  if (!quorum) throw internal('db-sync records no committee quorum');

  const certs = new Map(certRows.map((row) => [row.cold_key_id, row]));

  const members = head.members.map((m): CommitteeMember => {
    // The seat started at the earliest step of the unbroken run of steps, ending
    // at the head, that all include this credential. Re-election with a new
    // expiry does not restart it; a removal or a NoConfidence does.
    const key = memberKey(m);
    let termStartEpoch: number | null = head.epoch;
    for (let i = steps.length - 1; i >= 0; i--) {
      const step = steps[i]!;
      if (!step.members.some((other) => memberKey(other) === key)) break;
      termStartEpoch = step.epoch;
    }
    const cert = certs.get(m.hashId);
    const resigned = cert?.resigned ?? false;
    const hot =
      !resigned && cert?.hot_hash != null && cert.hot_is_script != null
        ? encodeCommitteeHotId(cert.hot_hash, cert.hot_is_script)
        : null;
    return {
      role: 'cc',
      coldCredential: encodeCommitteeColdId(m.hash, m.isScript),
      hotCredential: hot,
      termStartEpoch,
      termExpiryEpoch: toNullableInt(m.expiry),
      hasResigned: resigned,
      isScriptBased: m.isScript,
    };
  });
  members.sort((a, b) => (a.coldCredential < b.coldCredential ? -1 : a.coldCredential > b.coldCredential ? 1 : 0));

  return { members, quorum, enactedBy: head.ref, isDissolved: head.dissolves };
}

export async function readCommittee(ctx: Ctx): Promise<Committee> {
  const [stepRows, certRows, conway] = await Promise.all([
    ctx.db.query<CommitteeStepRow>(COMMITTEE_STEPS_SQL),
    ctx.db.query<CertRow>(COMMITTEE_CERTS_SQL),
    ctx.db.query<{ epoch: number | null }>(CONWAY_START_SQL),
  ]);
  return assembleCommittee(stepRows, certRows, toNullableInt(conway[0]?.epoch ?? null));
}
