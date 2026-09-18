import type {
  Committee,
  CommitteeApi,
  CommitteeMember,
  Constitution,
  Envelope,
  PagedEnvelope,
  PageRequest,
} from '@govtool/data-providers/chain-data';

import { internal, notFound } from '../../common/errors';
import { encodeCip129CcHotId } from '../../common/ids';
import { envelope } from '../../common/meta';
import { paginateLocally } from '../../common/paging';
import type { KoiosHttpClient } from '../../http/client';
import { projectConstitution } from '../../mappers/metadata.mapper';
import { mapBody, mapPreviousAction } from '../../mappers/proposal.mapper';
import type {
  CommitteeInfoRow,
  CommitteeMemberEntry,
  ProposalRow,
} from '../../rows';

export class KoiosCommitteeApi implements CommitteeApi {
  constructor(private readonly http: KoiosHttpClient) {}

  /**
   * `/committee_info` reads current membership from ledger gov-state, which
   * is what the contract asks for — authoritative membership rather than a
   * replay of enacted `UpdateCommittee` actions. It is the whole committee in
   * one request, including the quorum and the action that last set it.
   */
  async getCommittee(): Promise<Envelope<Committee>> {
    const row = await this.committeeRow();
    return envelope({
      members: row.members.map(mapMember),
      quorum: {
        numerator: row.quorum_numerator,
        denominator: row.quorum_denominator,
      },
      enactedBy:
        row.proposal_id === null
          ? null
          : {
              id: row.proposal_id,
              txHash: row.proposal_tx_hash ?? '',
              index: row.proposal_index ?? 0,
            },
    });
  }

  /** Matched against either credential: callers hold the hot or the cold id. */
  async getMember(id: string): Promise<Envelope<CommitteeMember>> {
    const row = await this.committeeRow();
    const entry = row.members.find(
      (member) =>
        member.cc_cold_id === id ||
        member.cc_hot_id === id ||
        member.cc_cold_hex === id ||
        member.cc_hot_hex === id,
    );
    if (entry === undefined) {
      throw notFound('No committee member with that credential', { id });
    }
    return envelope(mapMember(entry));
  }

  /**
   * Reconstructed from the latest enacted `NewConstitution` action, because
   * Koios has no constitution endpoint at all.
   *
   * The contract prefers gov-state over replaying enacted actions, and this
   * is a replay — but it is a replay of exactly one action, the most recent
   * enacted one, so the failure mode the contract warns about (accumulating
   * error over a chain of actions) does not apply. What is genuinely missing
   * is the constitution *document*: the anchor is on chain, its text is not,
   * and fetching it is the Metadata Service's job.
   */
  async getConstitution(): Promise<Envelope<Constitution>> {
    const rows = await this.constitutionRows({ limit: 1 });
    const row = rows[0];
    if (row === undefined) {
      throw notFound('No enacted NewConstitution action on this network');
    }
    return envelope(mapConstitution(row));
  }

  async listConstitutionHistory(
    q?: PageRequest,
  ): Promise<PagedEnvelope<Constitution>> {
    const rows = await this.constitutionRows({});
    return envelope(paginateLocally(rows.map(mapConstitution), q));
  }

  private async committeeRow(): Promise<CommitteeInfoRow> {
    const response = await this.http.get<CommitteeInfoRow>('committee_info');
    const row = response.rows[0];
    if (row === undefined) {
      throw internal('Koios returned no committee information.');
    }
    return row;
  }

  private async constitutionRows(q: {
    limit?: number;
  }): Promise<ProposalRow[]> {
    const response = await this.http.get<ProposalRow>(
      'proposal_list',
      {
        proposal_type: 'eq.NewConstitution',
        enacted_epoch: 'not.is.null',
      },
      { order: 'enacted_epoch.desc', limit: q.limit },
    );
    return response.rows;
  }
}

/**
 * The deployment sends `cc_cold_id` / `cc_hot_id` in CIP-129 bech32; the
 * published spec documents only the hex. Where the bech32 is absent it is
 * encoded from the hash, which is why `encodeCip129CcHotId` exists.
 */
function mapMember(entry: CommitteeMemberEntry): CommitteeMember {
  const hotHex = entry.cc_hot_hex;
  const hotIsScript = entry.cc_hot_has_script ?? false;
  const id =
    entry.cc_hot_id ??
    (hotHex === null
      ? (entry.cc_cold_id ?? entry.cc_cold_hex)
      : encodeCip129CcHotId(hotHex, hotIsScript));

  return {
    role: 'cc',
    id,
    hash: hotHex ?? entry.cc_cold_hex,
    isScriptBased: entry.cc_cold_has_script,
    coldCredential: {
      hash: entry.cc_cold_hex,
      isScriptBased: entry.cc_cold_has_script,
    },
    hotCredential:
      hotHex === null ? null : { hash: hotHex, isScriptBased: hotIsScript },
    // Koios reports when a term ends but never when it began.
    termStartEpoch: null,
    termExpiryEpoch: entry.expiration_epoch,
    hasResigned: entry.status === 'resigned',
  };
}

function mapConstitution(row: ProposalRow): Constitution {
  const body = mapBody(row);
  const anchor =
    body?.type === 'NewConstitution' ? body.anchor : { url: '', dataHash: '' };

  return {
    anchor,
    guardrailsScriptHash:
      body?.type === 'NewConstitution'
        ? (body.guardrailsScriptHash ?? null)
        : null,
    enactedBy: mapPreviousAction(row.proposal_id),
    enactedAt: row.enacted_epoch === null ? null : { epoch: row.enacted_epoch },
    document: projectConstitution({
      url: anchor.url === '' ? null : anchor.url,
      hash: anchor.dataHash,
      json: undefined,
    }),
  };
}
