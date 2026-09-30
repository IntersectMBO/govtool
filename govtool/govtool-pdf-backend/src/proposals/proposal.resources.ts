// Wire descriptors of SPEC §5.2 (proposal side). Owned by the proposals
// module; the item shape of /proposals (§8.2) is assembled from these.

import { col, defineResource } from '../query/resource';
import { GovernanceActionTypeResource } from '../lookups/lookups.resources';

export const ProposalResource = defineResource({
  name: 'proposal',
  scalars: {
    user_id: col.legacyId('userId'),
    prop_likes: col.int('likes'),
    prop_dislikes: col.int('dislikes'),
    prop_comments_number: col.int('commentsNumber'),
  },
});

export const ProposalConstitutionContentResource = defineResource({
  name: 'proposal-constitution-content',
  scalars: {
    prop_constitution_url: col.str('constitutionUrl'),
    prop_have_guardrails_script: col.bool('haveGuardrailsScript', true),
    prop_guardrails_script_url: col.str('guardrailsScriptUrl'),
    prop_guardrails_script_hash: col.str('guardrailsScriptHash'),
  },
});

export const ProposalHardForkContentResource = defineResource({
  name: 'proposal-hard-fork-content',
  scalars: {
    previous_ga_hash: col.str('previousGaHash'),
    previous_ga_id: col.str('previousGaId'),
    major: col.str('major'),
    minor: col.str('minor'),
  },
});

/** One revision; /proposals lists these and wraps each in its proposal. */
export const ProposalContentResource = defineResource({
  name: 'proposal-content',
  scalars: {
    proposal_id: col.legacyId('proposalId'),
    prop_rev_active: col.bool('revActive'),
    prop_abstract: col.str('abstract', false),
    prop_motivation: col.str('motivation', false),
    prop_rationale: col.str('rationale', false),
    gov_action_type_id: col.legacyId('govActionTypeId'),
    prop_name: col.str('name', false),
    is_draft: col.bool('isDraft'),
    user_id: col.legacyId('userId'),
    prop_submitted: col.bool('submitted'),
    prop_submission_tx_hash: col.str('submissionTxHash'),
    prop_submission_date: col.date('submissionDate'),
    is_locked: col.bool('isLocked'),
  },
  relations: {
    proposal: {
      field: 'proposal',
      target: () => ProposalResource,
      many: false,
      fk: 'proposalId',
      nullable: false,
    },
    gov_action_type: {
      field: 'govActionType',
      target: () => GovernanceActionTypeResource,
      many: false,
      fk: 'govActionTypeId',
      nullable: false,
    },
    proposal_constitution_content: {
      field: 'constitutionContent',
      target: () => ProposalConstitutionContentResource,
      many: false,
    },
    proposal_hard_fork_content: {
      field: 'hardForkContent',
      target: () => ProposalHardForkContentResource,
      many: false,
      fk: 'hardForkContentId',
    },
  },
  components: {
    proposal_links: {
      field: 'links',
      scalars: { prop_link: col.str('link', false), prop_link_text: col.str('text') },
    },
    proposal_withdrawals: {
      field: 'withdrawals',
      scalars: {
        prop_receiving_address: col.str('receivingAddress'),
        prop_amount: col.float('amount'),
      },
    },
  },
});

export const ProposalVoteResource = defineResource({
  name: 'proposal-vote',
  scalars: {
    proposal_id: col.legacyId('proposalId'),
    user_id: col.legacyId('userId'),
    vote_result: col.bool('voteResult'),
  },
});
