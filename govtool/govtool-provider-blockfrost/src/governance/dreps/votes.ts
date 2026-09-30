/**
 * A DRep's vote listing (SPEC.md §5.3): every action it voted on, plus every
 * action that was votable while it was registered — the same window rules as
 * the db-sync provider.
 *
 *   Window    from the DRep's FIRST registration to its retirement (while the
 *             newest registration/retirement certificate is one) or now.
 *   Votable   an action is open in epochs [submitted, lastVotable], where
 *             lastVotable is the epoch before it was ratified or dropped, or
 *             the epoch it expired in. Still-open actions have no end.
 *   Bootstrap during the Conway bootstrap phase (protocol < 10) the ledger
 *             rejects DRep votes on anything but an InfoAction, so a non-Info
 *             action counts only if it was open in a post-bootstrap epoch of
 *             the window. The bootstrap end is the enactment epoch of the
 *             HardForkInitiation to protocol 10, read off the proposals.
 *   Voted rows are always included, so voted is a subset of the listing.
 *
 * Cost: the proposal index and every record (160 requests on mainnet), the
 * DRep's `/votes` pages, `/updates`, and one dated read for the registration
 * — plus, for a retired DRep only, a dated read per candidate action, since
 * only then can an action be submitted after the window closed.
 */
import type { GovActionType, VoteChoice } from '@govtool/data-providers/chain-data';

import { loadClock, txStamp } from '../../chain';
import type { Session } from '../../context';
import { internal } from '../../errors';
import { decodeRecord, loadIndex, requireRecord, type ProposalRecord } from '../proposals/records';
import { loadCurrentParams } from '../../network';
import { indexCertificates, loadUpdates } from './registration';

interface BfDRepVote {
  tx_hash: string;
  cert_index: number;
  proposal_id?: string;
  proposal_tx_hash: string;
  proposal_cert_index: number;
  vote: string;
}

export interface VotableRow {
  action: { id: string; txHash: string; index: number; type: GovActionType };
  vote?: { choice: VoteChoice; txHash: string; certIndex: number };
}

const CHOICES: Record<string, VoteChoice> = { yes: 'yes', no: 'no', abstain: 'abstain' };

/** db-sync's COALESCE(ratified - 1, dropped - 1, expired): null while still open. */
const lastVotable = (r: ProposalRecord): number | null =>
  r.ratified !== null ? r.ratified - 1 : r.dropped !== null ? r.dropped - 1 : r.expired;

export async function bootstrapEnd(s: Session, records: readonly ProposalRecord[]): Promise<number> {
  let end = Infinity;
  for (const r of records) {
    if (r.type !== 'HardForkInitiation' || r.enacted === null) continue;
    const { body } = decodeRecord(r, 'mainnet');
    if (body.type === 'HardForkInitiation' && body.protocolVersion.major >= 10) end = Math.min(end, r.enacted);
  }
  if (end !== Infinity) return end;
  // No hard fork to 10 on record: a network born past bootstrap, or one still in it.
  const params = await loadCurrentParams(s);
  return Number(params.protocol_major_ver) >= 10 ? 0 : Infinity;
}

/** Oldest action first. */
export async function loadDRepVotable(s: Session, drepId: string): Promise<VotableRow[]> {
  const [index, updates, voteRows, clock] = await Promise.all([
    loadIndex(s),
    loadUpdates(s, drepId),
    s.http.getAll<BfDRepVote>(`/governance/dreps/${drepId}/votes`, { batch: 2 }),
    loadClock(s),
  ]);
  const records = await Promise.all(index.map((e) => requireRecord(s, e)));
  const certs = indexCertificates(updates, drepId);
  const [start, end] = await Promise.all([
    txStamp(s, certs.firstRegistration.tx_hash).then((at) => at.epoch),
    certs.retirement ? txStamp(s, certs.retirement.tx_hash).then((at) => at.epoch) : Promise.resolve(clock.epoch),
  ]);
  const bootstrap = await bootstrapEnd(s, records);

  // The newest vote per action wins (rows come oldest first).
  const votes = new Map<string, NonNullable<VotableRow['vote']>>();
  for (const v of voteRows) {
    const choice = CHOICES[v.vote];
    if (!choice) throw internal(`Blockfrost reported an unknown vote '${v.vote}'`);
    votes.set(`${v.proposal_tx_hash}#${v.proposal_cert_index}`, { choice, txHash: v.tx_hash, certIndex: v.cert_index });
  }

  const candidates: { entry: (typeof index)[number]; record: ProposalRecord }[] = [];
  const rows: (VotableRow | undefined)[] = index.map((entry, i) => {
    const record = records[i]!;
    const action = { id: entry.id, txHash: entry.txHash, index: entry.index, type: entry.type };
    const vote = votes.get(`${entry.txHash}#${entry.index}`);
    if (vote) return { action, vote };
    const last = lastVotable(record);
    const openInWindow = last === null || last >= start;
    const bootstrapOk = entry.type === 'InfoAction' || (end >= bootstrap && (last === null || last >= bootstrap));
    if (!openInWindow || !bootstrapOk) return undefined;
    if (certs.retirement) candidates.push({ entry, record });
    return { action };
  });
  for (const v of votes.keys()) {
    if (!index.some((e) => `${e.txHash}#${e.index}` === v)) throw internal('DRep voted on an action missing from the proposal index', { action: v });
  }

  // Only a retired DRep's window can close before an action was submitted.
  if (candidates.length) {
    const late = new Set<string>();
    await Promise.all(
      candidates.map(async ({ entry }) => {
        if ((await txStamp(s, entry.txHash)).epoch > end) late.add(entry.id);
      }),
    );
    return rows.filter((r): r is VotableRow => r !== undefined && (r.vote !== undefined || !late.has(r.action.id)));
  }
  return rows.filter((r): r is VotableRow => r !== undefined);
}
