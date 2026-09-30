// Wire descriptors of SPEC §5.4 (budget side). `bd_contact_information` is
// deliberately absent: stored, never serialized or populatable (Δ11).

import { col, defineResource } from '../query/resource';
import {
  BdContractTypeResource,
  BdCurrencyResource,
  BdIntersectCommitteeResource,
  BdRoadMapResource,
  BdTypeResource,
  CountryListResource,
} from '../lookups/lookups.resources';
import { PublicUserResource } from '../users/user.resource';

export const BdCostingResource = defineResource({
  name: 'bd-costing',
  scalars: {
    cost_breakdown: col.str('costBreakdown'),
    ada_amount: col.str('adaAmount'),
    amount_in_preferred_currency: col.str('amountInPreferredCurrency'),
    usd_to_ada_conversion_rate: col.str('usdToAdaConversionRate'),
    ada_amount_clone: col.float('adaAmountClone', false),
    amount_in_preferred_currency_clone: col.float('amountInPreferredCurrencyClone', false),
    usd_to_ada_conversion_rate_clone: col.float('usdToAdaConversionRateClone', false),
  },
  relations: {
    preferred_currency: {
      field: 'preferredCurrency',
      target: () => BdCurrencyResource,
      many: false,
      fk: 'preferredCurrencyId',
    },
  },
});

export const BdProposalDetailResource = defineResource({
  name: 'bd-proposal-detail',
  scalars: {
    proposal_name: col.str('proposalName'),
    proposal_description: col.str('proposalDescription'),
    key_dependencies: col.str('keyDependencies'),
    maintain_and_support: col.str('maintainAndSupport'),
    key_proposal_deliverables: col.str('keyProposalDeliverables'),
    resourcing_duration_estimates: col.str('resourcingDurationEstimates'),
    experience: col.str('experience'),
    other_contract_type: col.str('otherContractType'),
  },
  relations: {
    contract_type_name: {
      field: 'contractType',
      target: () => BdContractTypeResource,
      many: false,
      fk: 'contractTypeId',
    },
  },
});

export const BdPsapbResource = defineResource({
  name: 'bd-psapb',
  scalars: {
    problem_statement: col.str('problemStatement'),
    proposal_benefit: col.str('proposalBenefit'),
    supplementary_endorsement: col.str('supplementaryEndorsement'),
    explain_proposal_roadmap: col.str('explainProposalRoadmap'),
  },
  relations: {
    type_name: { field: 'type', target: () => BdTypeResource, many: false, fk: 'typeId' },
    roadmap_name: { field: 'roadmap', target: () => BdRoadMapResource, many: false, fk: 'roadmapId' },
    committee_name: {
      field: 'committee',
      target: () => BdIntersectCommitteeResource,
      many: false,
      fk: 'committeeId',
    },
  },
});

export const BdProposalOwnershipResource = defineResource({
  name: 'bd-proposal-ownership',
  scalars: {
    agreed: col.bool('agreed', true),
    group_name: col.str('groupName'),
    company_name: col.str('companyName'),
    type_of_group: col.str('typeOfGroup'),
    social_handles: col.str('socialHandles'),
    submited_on_behalf: col.str('submitedOnBehalf'),
    company_domain_name: col.str('companyDomainName'),
    proposal_public_champion: col.str('proposalPublicChampion'),
    key_info_to_identify_group: col.str('keyInfoToIdentifyGroup'),
  },
  relations: {
    be_country: { field: 'beCountry', target: () => CountryListResource, many: false, fk: 'beCountryId' },
  },
});

export const BdFurtherInformationResource = defineResource({
  name: 'bd-further-information',
  scalars: {},
  components: {
    proposal_links: {
      field: 'links',
      scalars: { prop_link: col.str('link', false), prop_link_text: col.str('text') },
    },
  },
});

/**
 * One BD version. Computed `master_proposal_created_at` and
 * `user_govtool_username` (§8.8) are added by the endpoint as `extra`.
 */
export const BdResource = defineResource({
  name: 'bd',
  scalars: {
    privacy_policy: col.bool('privacyPolicy'),
    intersect_named_administrator: col.bool('intersectNamedAdministrator'),
    intersect_admin_further_text: col.str('intersectAdminFurtherText'),
    prop_comments_number: col.int('commentsNumber'),
    is_active: col.bool('isActive'),
    master_id: col.legacyId('masterId', true),
    submitted_for_vote: col.datetime('submittedForVote'),
  },
  relations: {
    creator: {
      field: 'creator',
      target: () => PublicUserResource,
      many: false,
      fk: 'creatorId',
      nullable: false,
    },
    bd_costing: { field: 'costing', target: () => BdCostingResource, many: false, fk: 'costingId' },
    bd_proposal_detail: {
      field: 'proposalDetail',
      target: () => BdProposalDetailResource,
      many: false,
      fk: 'proposalDetailId',
    },
    bd_psapb: { field: 'psapb', target: () => BdPsapbResource, many: false, fk: 'psapbId' },
    bd_proposal_ownership: {
      field: 'proposalOwnership',
      target: () => BdProposalOwnershipResource,
      many: false,
      fk: 'proposalOwnershipId',
    },
    bd_further_information: {
      field: 'furtherInformation',
      target: () => BdFurtherInformationResource,
      many: false,
      fk: 'furtherInformationId',
    },
  },
});

export const BdPollResource = defineResource({
  name: 'bd-poll',
  scalars: {
    bd_proposal_id: col.legacyId('bdMasterId'),
    poll_yes: col.int('yes'),
    poll_no: col.int('no'),
    is_poll_active: col.bool('isActive'),
  },
});

export const BdPollVoteResource = defineResource({
  name: 'bd-poll-vote',
  scalars: {
    bd_poll_id: col.legacyId('bdPollId'),
    user_id: col.legacyId('userId'),
    vote_result: col.bool('voteResult'),
    drep_id: col.str('drepId', false),
    drep_voting_power: col.str('drepVotingPower', false),
  },
});

export const BdDraftResource = defineResource({
  name: 'bd-draft',
  scalars: { draft_data: col.json('draftData') },
  relations: {
    creator: {
      field: 'creator',
      target: () => PublicUserResource,
      many: false,
      fk: 'creatorId',
      nullable: false,
    },
  },
});
