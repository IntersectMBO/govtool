// Query allowlists of the proposal-side list routes (SPEC §8.2, §8.4).

import { scalarPaths } from '../query/resource';
import { QueryAllowlist } from '../query/types';
import { ProposalContentResource, ProposalVoteResource } from './proposal.resources';

const PROPOSAL_COUNTERS = ['proposal.prop_likes', 'proposal.prop_dislikes', 'proposal.prop_comments_number'];

/**
 * GET /api/proposals, on proposal-content rows. `prop_id` is virtual: the
 * endpoint rewrites it to `proposal_id`. Populate is accepted and ignored
 * (the item shape is fixed); `fields` gives V.
 */
export const PROPOSALS_ALLOWLIST: QueryAllowlist = {
  resource: ProposalContentResource,
  filterable: [...scalarPaths(ProposalContentResource), 'prop_id', ...PROPOSAL_COUNTERS],
  virtualFilters: { prop_id: 'int' },
  sortable: [...scalarPaths(ProposalContentResource), ...PROPOSAL_COUNTERS],
  populatable: [],
  ignoredPopulate: [
    'proposal_links',
    'proposal_withdrawals',
    'proposal_constitution_content',
    'proposal_hard_fork_content',
    'proposal',
  ],
  fields: false,
};

/** GET /api/proposal-votes. A client `user_id` is accepted, then dropped (Δ29). */
export const PROPOSAL_VOTES_ALLOWLIST: QueryAllowlist = {
  resource: ProposalVoteResource,
  filterable: ['proposal_id', 'user_id'],
  sortable: scalarPaths(ProposalVoteResource),
};
