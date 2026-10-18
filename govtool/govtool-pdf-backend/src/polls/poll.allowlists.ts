import { scalarPaths } from '../query/resource';
import { QueryAllowlist } from '../query/types';
import { PollResource, PollVoteResource } from './poll.resources';

/** GET /api/polls (§8.5). */
export const POLLS_ALLOWLIST: QueryAllowlist = {
  resource: PollResource,
  filterable: scalarPaths(PollResource),
  sortable: scalarPaths(PollResource),
};

/** GET /api/poll-votes (§8.6). `user_id` accepted, then forced to the caller. */
export const POLL_VOTES_ALLOWLIST: QueryAllowlist = {
  resource: PollVoteResource,
  filterable: ['poll_id', 'vote_result', 'user_id'],
  sortable: scalarPaths(PollVoteResource),
};
