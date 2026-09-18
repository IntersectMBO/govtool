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
 * Not implemented. Blockfrost has no committee or constitution resource at
 * all — `/governance/committee` and `/governance/constitution` answer 400
 * "Invalid path".
 *
 * Both are reconstructible in principle by replaying enacted
 * `UpdateCommittee` and `NewConstitution` actions, which this provider can
 * list. That is deliberately not done here: the contract's own note on
 * `committee.ts` is that gov-state is authoritative and replay is
 * error-prone, so a replayed answer would be presented as fact while being a
 * guess.
 */
export class BlockfrostCommitteeApi implements CommitteeApi {
  getCommittee(): Promise<Envelope<Committee>> {
    return Promise.reject(
      unsupported(
        'governance.committee.getCommittee',
        'Blockfrost has no committee resource; replaying UpdateCommittee actions would be a guess, not gov-state',
      ),
    );
  }

  getMember(_id: string): Promise<Envelope<CommitteeMember>> {
    return Promise.reject(
      unsupported(
        'governance.committee.getMember',
        'Blockfrost has no committee resource',
      ),
    );
  }

  getConstitution(): Promise<Envelope<Constitution>> {
    return Promise.reject(
      unsupported(
        'governance.committee.getConstitution',
        'Blockfrost has no constitution resource',
      ),
    );
  }

  listConstitutionHistory(
    _q?: PageRequest,
  ): Promise<PagedEnvelope<Constitution>> {
    return Promise.reject(
      unsupported('governance.committee.listConstitutionHistory'),
    );
  }
}
