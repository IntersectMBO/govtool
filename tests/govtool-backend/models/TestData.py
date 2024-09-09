from typing import TypedDict, Optional, List, Dict, Any


class ProposalListResponse(TypedDict):
    page: int
    pageSize: int
    total: int
    elements: List["Proposal"]


class GetProposalResponse(TypedDict):
    votes: int
    proposal: "Proposal"


class Proposal(TypedDict):
    id: str
    txHash: str
    index: int
    type: str
    details: Optional[dict]
    expiryDate: str
    expiryEpochNo: int
    createdDate: str
    createdEpochNo: int
    url: str
    metadataHash: str
    title: Optional[str]
    abstract: Optional[str]
    motivation: Optional[str]
    rationale: Optional[str]
    dRepYesVotes: int
    dRepNoVotes: int
    dRepAbstainVotes: int
    ccYesVotes: int
    ccNoVotes: int
    ccAbstainVotes: int
    poolYesVotes: int
    poolNoVotes: int
    poolAbstainVotes: int


class Drep(TypedDict):
    drepId: str
    url: str
    metadataHash: str
    deposit: int


class Delegation(TypedDict):
    stakeKey: str
    dRepId: str


class AdaHolder(TypedDict):
    stakeKey: str


class Vote(TypedDict):
    proposalId: str
    drepId: str
    vote: str
    url: str
    metadataHash: str


class VoteonProposal(TypedDict):
    vote: Vote
    proposal: Proposal


class DrepInfo(TypedDict):
    isRegisteredAsDRep: bool
    wasRegisteredAsDRep: bool
    isRegisteredAsSoleVoter: bool
    wasRegisteredAsSoleVoter: bool
    deposit: int
    url: str
    dataHash: str
    votingPower: Optional[int]
    dRepRegisterTxHash: str
    dRepRetireTxHash: Optional[str]
    soleVoterRegisterTxHash: Optional[str]
    soleVoterRetireTxHash: Optional[str]


class EpochParam(TypedDict):
    block_id: int
    coins_per_utxo_size: int
    collateral_percent: int
    committee_max_term_length: int
    committee_min_size: int
    cost_model_id: int
    decentralisation: int
    drep_activity: int
    drep_deposit: int
    dvt_committee_no_confidence: float
    dvt_committee_normal: float
    dvt_hard_fork_initiation: float
    dvt_motion_no_confidence: float
    dvt_p_p_economic_group: float
    dvt_p_p_gov_group: float
    dvt_p_p_network_group: float
    dvt_p_p_technical_group: float
    dvt_treasury_withdrawal: float
    dvt_update_to_constitution: float
    epoch_no: int
    extra_entropy: Optional[int]
    gov_action_deposit: int
    gov_action_lifetime: int
    id: int
    influence: float
    key_deposit: int
    max_bh_size: int
    max_block_ex_mem: int
    max_block_ex_steps: int
    max_block_size: int
    max_collateral_inputs: int
    max_epoch: int
    max_tx_ex_mem: int


class TxStatus(TypedDict):
    transactionConfirmed: bool


class NetworkMetrics(TypedDict):
    currentTime: str
    currentEpoch: int
    currentBlock: int
    uniqueDelegators: int
    totalDelegations: int
    totalGovernanceActions: int
    totalDRepVotes: int
    totalRegisteredDReps: int
    alwaysAbstainVotingPower: int
    alwaysNoConfidenceVotingPower: int
