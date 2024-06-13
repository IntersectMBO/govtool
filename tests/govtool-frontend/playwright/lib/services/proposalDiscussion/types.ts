type ProposalLinksType = {
  prop_link: string;
  prop_link_text: string;
};

export type ProposalCreateRequest = {
  proposal_links: Array<ProposalLinksType>;
  gov_action_type_id: number;
  prop_name: string;
  prop_abstract: string;
  prop_motivation: string;
  prop_rationale: string;
  prop_receiving_address: string;
  prop_amount: string;
  is_draft: boolean;
};

export type CommentRequest = {
  proposal_id: string;
  comment_text: string;
};

export type PollRequest = {
  proposal_id: string;
  poll_start_dt: string;
  is_poll_active: boolean;
};
