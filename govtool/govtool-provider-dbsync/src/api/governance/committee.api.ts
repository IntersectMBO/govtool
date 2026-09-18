import type {
  Committee,
  CommitteeApi,
  CommitteeMember,
  Constitution,
  Envelope,
  PagedEnvelope,
  PageRequest,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';

/**
 * Not implemented. `get-network-metrics.sql` counts committee members and
 * reads the quorum, and that count is served at `governance.metrics.get`;
 * there is no statement that returns the members themselves or the
 * constitution.
 */
export class DbSyncCommitteeApi implements CommitteeApi {
  getCommittee(): Promise<Envelope<Committee>> {
    return Promise.reject(unsupported('governance.committee.getCommittee'));
  }

  getMember(_id: string): Promise<Envelope<CommitteeMember>> {
    return Promise.reject(unsupported('governance.committee.getMember'));
  }

  getConstitution(): Promise<Envelope<Constitution>> {
    return Promise.reject(unsupported('governance.committee.getConstitution'));
  }

  listConstitutionHistory(
    _q?: PageRequest,
  ): Promise<PagedEnvelope<Constitution>> {
    return Promise.reject(
      unsupported('governance.committee.listConstitutionHistory'),
    );
  }
}
