import { CardanoTestWalletJson } from "@cardanoapi/cardano-test-wallet/types";

export type StaticWallet = CardanoTestWalletJson & {
  dRepId: string;
  address: string;
  givenName?: string;
};

export type KuberValue = {
  [policyId: string]: Record<string, BigInt | number> | BigInt | number;
};

export interface PaginatedLiveProposal {
  page: number;
  pageSize: number;
  total: number;
  elements: IProposal[];
}

export interface IProposal {
  id: string;
  txHash: string;
  index: number;
  type: string;
  details: any;
  expiryDate: string;
  expiryEpochNo: number;
  createdDate: string;
  createdEpochNo: number;
  url: string;
  metadataHash: string;
  protocolParams: EpochParams | null;
  title: string | null;
  about: string | null;
  motivation: string | null;
  rationale: string | null;
  metadata: any;
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
  prevGovActionIndex: null | number;
  prevGovActionTxHash: null | string;
}

export type IVote = {
  drepId: string;
  metadataHash: string;
  url: string;
  proposalId: string;
  vote: string; // You might want to consider using a more specific type, like 'VoteType' enum
};

export type IVotedProposal = {
  proposal: IProposal;
  vote: IVote;
};

export type IDRepInfo = {
  name: string;
  objectives?: string;
  motivations?: string;
  qualifications?: string;
  paymentAddress?: string;
  identityReferenceLinks?: LinkType[];
  linksReferenceLinks?: LinkType[];
  donNotList?: boolean;
};

export type LinkType = {
  url: string;
  description: string;
};

export enum ProposalType {
  info = "Info Action",
  treasury = "Treasury requests",
  updatesToTheConstitution = "Updates to the Constitution",
  motionOfNoConfedence = "Motion of No Confidence",
}

export enum BootstrapGovernanceActionType {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  HardFork = "HardForkInitiation",
}

export enum GovernanceActionType {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  TreasuryWithdrawal = "TreasuryWithdrawals",
  HardFork = "HardForkInitiation",
  NoConfidence = "NoConfidence",
  NewCommittee = "NewCommittee",
  UpdatetotheConstitution = "NewConstitution",
}

export enum outcomeType {
  NewConstitution = "New Constitution",
  NewCommittee = "Update Committee",
  HardForkInitiation = "Hard-Fork Initiation",
  NoConfidence = "Motion of no Confidence",
  InfoAction = "Info Action",
  TreasuryWithdrawals = "Treasury Withdrawals",
  ParameterChange = "Protocol Parameter Change",
}

export enum FullGovernanceDRepVoteActionsType {
  ProtocolParameterChange = "ParameterChange",
  InfoAction = "InfoAction",
  TreasuryWithdrawal = "TreasuryWithdrawals",
  HardFork = "HardForkInitiation",
}

export type DRepStatus = "Active" | "Inactive" | "Retired";

export interface PaginatedDRepResponse {
  page: number;
  pageSize: number;
  total: number;
  elements: IDRep[];
}

export type IDRep = {
  isScriptBased: boolean;
  drepId: string;
  view: string;
  url: string;
  metadataHash: string;
  deposit: number;
  votingPower: number;
  status: DRepStatus;
  type: string;
  latestTxHash: string;
  givenName: string | null;
  latestRegistrationDate: string;
};

export type ProtocolParams = {
  dRepDeposit: number;
  govActionDeposit: number;
  protocolVersion: ProtocolVersionType;
};

type Comment = {
  proposal_id: string;
  comment_text: string;
};

export type StaticProposal = {
  id: number;
  comments?: Comment[];
  title: string;
};

export type CommentResponse = {
  id: number;
  attributes: {
    proposal_id: string;
    comment_parent_id: null | string;
    user_id: string;
    comment_text: string;
    createdAt: string;
    updatedAt: string;
    bd_proposal_id: string | null;
    drep_id: string | null;
    comments_reports: any;
    user_govtool_username: string;
    subcommens_number: number;
  };
};

export type ProposalLink = {
  prop_link: string;
  prop_link_text: string;
};

export type ProposalCreateRequest = {
  proposal_links: Array<ProposalLink>;
  gov_action_type_id: number;
  prop_name: string;
  prop_abstract: string;
  prop_motivation: string;
  prop_rationale: string;
  prop_receiving_address?: string;
  prop_amount?: string;
  prop_constitution_url?: string;
  prop_guardrails_script_url?: string;
  prop_guardrails_script_hash?: string;
  has_guardrails?: boolean;
  is_draft: boolean;
};

export type ProposedGovAction = {
  id: number;
  attributes: {
    gov_action_type_name: string;
    createdAt: string;
    updatedAt: string;
  };
};

export type WalletAndAnchorType = {
  url: string;
  dataHash: string;
  wallet: StaticWallet;
};

export type ProtocolVersionType = {
  major: number;
  minor: number;
};

export type EpochParams = {
  block_id: number | null;
  coins_per_utxo_size: number | null;
  collateral_percent: number | null;
  committee_max_term_length: number | null;
  committee_min_size: number | null;
  cost_model_id: number | null;
  decentralisation: number | null;
  drep_activity: number | null;
  drep_deposit: number | null;
  dvt_committee_no_confidence: number | null;
  dvt_committee_normal: number | null;
  dvt_hard_fork_initiation: number | null;
  dvt_motion_no_confidence: number | null;
  dvt_p_p_economic_group: number | null;
  dvt_p_p_gov_group: number | null;
  dvt_p_p_network_group: number | null;
  dvt_p_p_technical_group: number | null;
  dvt_treasury_withdrawal: number | null;
  dvt_update_to_constitution: number | null;
  epoch_no: number | null;
  extra_entropy: null;
  gov_action_deposit: number | null;
  gov_action_lifetime: number | null;
  id: number;
  influence: number | null;
  key_deposit: number | null;
  max_bh_size: number | null;
  max_block_ex_mem: number | null;
  max_block_ex_steps: number | null;
  max_block_size: number | null;
  max_collateral_inputs: number | null;
  max_epoch: number | null;
  max_tx_ex_mem: number | null;
  max_tx_ex_steps: number | null;
  max_tx_size: number | null;
  max_val_size: number | null;
  min_fee_a: number | null;
  min_fee_b: number | null;
  min_fee_ref_script_cost_per_byte: number | null;
  min_pool_cost: number | null;
  min_utxo_value: number | null;
  monetary_expand_rate: number | null;
  nonce: string | null;
  optimal_pool_count: number | null;
  pool_deposit: number | null;
  price_mem: number | null;
  price_step: number | null;
  protocol_major: number | null;
  protocol_minor: number | null;
  pvt_committee_no_confidence: number | null;
  pvt_committee_normal: number | null;
  pvt_hard_fork_initiation: number | null;
  pvt_motion_no_confidence: number | null;
  pvtpp_security_group: number | null;
  treasury_growth_rate: number | null;
};

export interface imageObject {
  "@type": "ImageObject";
  contentUrl: string;
  sha256: string;
}

export interface outcomeProposal {
  id: string;
  tx_hash: string;
  index: string;
  type: string;
  yes_votes: string;
  no_votes: string;
  abstain_votes: string;
  description: any;
  expiry_date: string;
  expiration: number;
  time: string;
  epoch_no: number;
  url: string;
  data_hash: string;
  title: string | null;
  abstract: string | null;
  motivation?: string | null;
  rationale?: string | null;
  pool_yes_votes?: string;
  pool_no_votes?: string;
  pool_abstain_votes?: string;
  cc_yes_votes?: string;
  cc_no_votes?: string;
  cc_abstain_votes?: string;
  proposal_params: EpochParams | null;
}

export interface outcomeMetadata {
  metadataStatus: string;
  metadataValid: boolean;
  data: outcomeMetadataBody;
}

interface outcomeMetadataBody {
  abstract: string;
  motivation: "string";
  rationale: string;
  title: string;
}

export interface InvalidMetadataType {
  type: string;
  reason: string;
  url: string;
  hash: string;
}

export enum BudgetDiscussionEnum {
  Core = "Core",
  Research = "Research",
  GovernanceSupport = "Governance Support",
  MarketingAndInnovation = "Marketing & Innovation",
  NoCategory = "No Category",
}

export interface BudgetProposalContactInformationProps {
  beneficiaryFullName: string;
  beneficiaryEmail: string;
  beneficiaryCountry: string;
  beneficiaryNationality: string;
  submissionLeadFullName: string;
  submissionLeadEmail: string;
}

export type CompanyType = "Individual" | "Company" | "Group";
export enum CompanyEnum {
  Individual = "Individual",
  Company = "Company",
  Group = "Group",
}

export interface BudgetProposalOwnershipProps {
  companyType: CompanyType;
  contactDetails: string;
  groupName?: string;
  groupType?: string;
  groupKeyIdentity?: string;
  companyName?: string;
  companyDomainName?: string;
  countryOfIncorportation?: string;
}

export type RoadmapNameType =
  | "Scaling the L1 Engine"
  | "Architectural Excellence"
  | "Leios"
  | "Incoming Liquidity"
  | "L2 Expansion"
  | "Programmable Assets"
  | "Multiple Node Implementations"
  | "SPO Incentive Improvements"
  | "It doesn't align"
  | "It supports the product roadmap"
  | "Developer / User Experience";

export enum RoadmapNameEnum {
  ScalingTheL1Engine = "Scaling the L1 Engine",
  ArchitecturalExcellence = "Architectural Excellence",
  Leios = "Leios",
  IncomingLiquidity = "Incoming Liquidity",
  L2Expansion = "L2 Expansion",
  ProgrammableAssets = "Programmable Assets",
  MultipleNodeImplementations = "Multiple Node Implementations",
  SPOIncentiveImprovements = "SPO Incentive Improvements",
  NoAlignment = "It doesn't align",
  SupportsProductRoadmap = "It supports the product roadmap",
  DeveloperUserExperience = "Developer / User Experience",
}

export type BudgetDiscussionType =
  | "Core"
  | "Research"
  | "Governance Support"
  | "Marketing & Innovation"
  | "None of these";

export enum CommitteeAlignmentEnum {
  TechnicalSteeringCommittee = "Technical Steering Committee",
  ProductCommittee = "Product Committee",
  OpenSourceCommittee = "Open Source Committee",
  CivicsCommittee = "Civics Committee",
  MembershipAndCommunityCommittee = "Membership & Community Committee",
  BudgetCommittee = "Budget Committee",
  MarketingCommittee = "Marketing Committee",
  Unsure = "Unsure",
  None = "None",
}

export type CommitteeAlignmentType =
  | "Technical Steering Committee"
  | "Product Committee"
  | "Open Source Committee"
  | "Civics Committee"
  | "Membership & Community Committee"
  | "Budget Committee"
  | "Marketing Committee"
  | "Unsure"
  | "None";

export enum LocationEnum {
  Nepal = "Nepal",
  Netherlands = "Netherlands",
  UnitedStates = "United States",
  UnitedKingdom = "United Kingdom",
  Canada = "Canada",
  Australia = "Australia",
  Germany = "Germany",
  France = "France",
  Japan = "Japan",
  SouthKorea = "South Korea",
}

export interface BudgetProposalProblemStatementAndBenefitProps {
  problemStatement: string;
  proposalBenefits: string;
  roadmapName: RoadmapNameType;
  productRoadmapDescription?: string;
  budgetDiscussionType: BudgetDiscussionType;
  committeeAlignmentType: CommitteeAlignmentType;
  suplimentaryEndorsement: string;
}

export type ProposalContractingType =
  | "Milestone Based Fixed Price"
  | "Time and Materials"
  | "Service Level Agreement"
  | "Other"
  | "Reimbursement"
  | "Intersect Procurement Process";

export enum ProposalContractingEnum {
  MilestoneBasedFixedPrice = "Milestone Based Fixed Price",
  TimeAndMaterials = "Time and Materials",
  ServiceLevelAgreement = "Service Level Agreement",
  Other = "Other",
  Reimbursement = "Reimbursement",
  IntersectProcurementProcess = "Intersect Procurement Process",
}

export interface BudgetProposalDetailsProps {
  proposalName: string;
  proposalDescription: string;
  proposalKeyDependencies: string;
  proposalMaintainAndSupport: string;
  milestones: string;
  teamSizeAndDuration: string;
  previousExperience: string;
  contracting: ProposalContractingType;
  otherDescription?: string;
}

export type preferredCurrencyType =
  | "United States Dollar"
  | "Euro"
  | "Japanese Yen"
  | "Australian Dollar"
  | "Nepalese Rupee";

export enum PreferredCurrencyEnum {
  USD = "United States Dollar",
  EUR = "Euro",
  JPY = "Japanese Yen",
  AUD = "Australian Dollar",
  NPR = "Nepalese Rupee",
}

export interface BudgetCostingProps {
  adaAmount: number;
  adaToUsdConversionRate: number;
  preferredCurrency: preferredCurrencyType;
  AmountInPreferredCurrency: number;
  costBreakdown: string;
}

export interface AdministrationAndAuditingProps {
  intersectAdministration: boolean;
  venderDetails: string;
}

export interface BudgetProposalProps {
  proposalOwnership: BudgetProposalOwnershipProps;
  problemStatementAndBenefits: BudgetProposalProblemStatementAndBenefitProps;
  proposalDetails: BudgetProposalDetailsProps;
  costing: BudgetCostingProps;
  furtherInformation: Array<ProposalLink>;
  administrationAndAuditing: AdministrationAndAuditingProps;
}

export enum BudgetProposalStageEnum {
  ProposalOwnership = 1,
  ProblemStatementAndBenefits = 2,
  ProposalDetails = 3,
  Costing = 4,
  FurtherInformation = 5,
  AdministrationAndAuditing = 6,
  Review = 7,
}
