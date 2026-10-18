import { col, defineResource } from '../query/resource';

/** §5.2 Poll. */
export const PollResource = defineResource({
  name: 'poll',
  scalars: {
    proposal_id: col.legacyId('proposalId'),
    poll_yes: col.int('yes'),
    poll_no: col.int('no'),
    poll_start_dt: col.datetime('startDt'),
    is_poll_active: col.bool('isActive'),
  },
});

/** §5.2 PollVote. */
export const PollVoteResource = defineResource({
  name: 'poll-vote',
  scalars: {
    poll_id: col.legacyId('pollId'),
    user_id: col.legacyId('userId'),
    vote_result: col.bool('voteResult'),
  },
});
