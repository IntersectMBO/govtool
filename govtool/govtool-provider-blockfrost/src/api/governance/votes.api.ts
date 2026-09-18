import type {
  Envelope,
  PagedEnvelope,
  VoteListQuery,
  VoteRecord,
  VotesApi,
} from '@govtool/data-providers/chain-data';

import { unsupported } from '../../common/errors';

/**
 * Not implemented.
 *
 * Blockfrost indexes votes two ways and neither answers this resource: per
 * proposal (`governance.proposals.listVotes` serves that) and per DRep, where
 * the response omits which proposal each vote was about. There is no
 * cross-cutting feed, and no route from a vote's transaction hash back to the
 * vote — `/txs/{hash}` answers 500 here in any case.
 */
export class BlockfrostVotesApi implements VotesApi {
  list(
    _q?: VoteListQuery & { voterId?: string; proposalId?: string },
  ): Promise<PagedEnvelope<VoteRecord>> {
    return Promise.reject(
      unsupported(
        'governance.votes.list',
        'Blockfrost has no cross-cutting vote feed; use governance.proposals.listVotes',
      ),
    );
  }

  get(_txHash: string, _q?: { index?: number }): Promise<Envelope<VoteRecord>> {
    return Promise.reject(
      unsupported(
        'governance.votes.get',
        'no Blockfrost route resolves a vote from its transaction hash',
      ),
    );
  }
}
