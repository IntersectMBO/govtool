import type {
  GovAction,
  VoteAggregate,
} from '@govtool/data-providers/chain-data';

import { ProposalService } from '../proposal/proposal.service';
import { ProposalResponse } from '../proposal/proposal.type';
import { compareFiguresDescending, voteFigure } from './vote-figures';

const aggregate = (
  role: VoteAggregate['role'],
  representation: VoteAggregate['representation'] = 'stake',
): VoteAggregate => ({
  role,
  representation,
  yes: '9007199254740993',
  no: '2',
  abstain: '3',
  notVoted: '4',
  totalEligible: '9007199254741002',
  threshold: { numerator: 2, denominator: 3 },
});

const action = (voteAggregates?: VoteAggregate[]) =>
  ({ voteAggregates }) as GovAction;

describe('voteFigure', () => {
  it('reads a served figure exactly', () => {
    const served = action([aggregate('drep')]);
    expect(voteFigure(served, 'drep', 'yes')).toBe(9007199254740993n);
    expect(voteFigure(served, 'drep', 'abstain')).toBe(3);
  });

  it('is null, not 0, when the provider serves no aggregates for the action', () => {
    expect(voteFigure(action(undefined), 'drep', 'yes')).toBeNull();
  });

  it('is null for a percent aggregate, which has no whole number', () => {
    expect(
      voteFigure(action([aggregate('drep', 'percent')]), 'drep', 'yes'),
    ).toBeNull();
  });

  it('is 0 for a role left out of served aggregates: it does not vote on the action', () => {
    expect(voteFigure(action([aggregate('drep')]), 'spo', 'yes')).toBe(0);
  });
});

describe('compareFiguresDescending', () => {
  it('orders known figures high to low and unknown ones last', () => {
    const figures = [null, 1, 9007199254740993n, null, 0];
    expect([...figures].sort(compareFiguresDescending)).toEqual([
      9007199254740993n,
      1,
      0,
      null,
      null,
    ]);
  });

  it('sorts proposals with unknown totals after every known total', () => {
    const service = new ProposalService(null!, null!, null);
    const known = {
      id: 'known',
      dRepYesVotes: 0,
      poolYesVotes: 0,
      ccYesVotes: 0,
    } as ProposalResponse;
    const unknown = {
      ...known,
      id: 'unknown',
      dRepYesVotes: null,
      poolYesVotes: null,
      ccYesVotes: null,
    };
    expect(
      service.sortProposals([unknown, known], 'MostYesVotes').map((p) => p.id),
    ).toEqual(['known', 'unknown']);
  });
});
