// Query allowlists of the budget-side routes (SPEC §8.8, §8.10, §8.11).

import { scalarPaths } from '../query/resource';
import { QueryAllowlist } from '../query/types';
import { BdDraftResource, BdPollResource, BdPollVoteResource, BdResource } from './budget.resources';

/** Populatable on every BD route (§8.8). `bd_contact_information` gives V. */
export const BD_POPULATABLE = [
  'creator',
  'bd_costing.preferred_currency',
  'bd_proposal_detail.contract_type_name',
  'bd_psapb.type_name',
  'bd_psapb.roadmap_name',
  'bd_psapb.committee_name',
  'bd_proposal_ownership.be_country',
  'bd_further_information',
];

/** GET /api/bds and GET /api/bds/:id. */
export const BDS_ALLOWLIST: QueryAllowlist = {
  resource: BdResource,
  filterable: [
    ...scalarPaths(BdResource),
    'creator',
    'creator.govtool_username',
    'bd_psapb.type_name.id',
    'bd_proposal_detail.proposal_name',
  ],
  sortable: [...scalarPaths(BdResource), 'bd_proposal_detail.proposal_name', 'creator.govtool_username'],
  populatable: BD_POPULATABLE,
  // A component path: accepted, no effect (components are always inline).
  ignoredPopulate: ['bd_further_information.proposal_links'],
};

/** GET /api/bd-polls (§8.11). */
export const BD_POLLS_ALLOWLIST: QueryAllowlist = {
  resource: BdPollResource,
  filterable: scalarPaths(BdPollResource),
  sortable: scalarPaths(BdPollResource),
};

/** GET /api/bd-poll-votes (§8.11). DRep votes are public by design. */
export const BD_POLL_VOTES_ALLOWLIST: QueryAllowlist = {
  resource: BdPollVoteResource,
  filterable: ['bd_poll_id', 'user_id', 'vote_result', 'drep_id'],
  sortable: scalarPaths(BdPollVoteResource),
};

/** GET /api/bd-drafts (§8.10); `creator` is forced to the caller. */
export const BD_DRAFTS_ALLOWLIST: QueryAllowlist = {
  resource: BdDraftResource,
  filterable: scalarPaths(BdDraftResource),
  sortable: scalarPaths(BdDraftResource),
  populatable: ['creator'],
};
