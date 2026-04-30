export type NetworkInfo = {
    current_epoch: number | string | null;
    current_block: number | string | null;
    network_name: string | null;
};


export type GetNetworkInfoResponse = {
    currentTime : string;
    epochNo: number;
    blockNo: number;
    networkName: string;
};


export type NetworkTotalStake = {
  total_stake_controlled_by_active_dreps: number | string;
  total_stake_controlled_by_spos: number | string;
  always_abstain_voting_power: number | string;
  always_no_confidence_voting_power: number | string;
};

export type GetNetworkTotalStakeResponse = {
  totalStakeControlledByDReps: number;
  totalStakeControlledBySPOs: number;
  alwaysAbstainVotingPower: number;
  alwaysNoConfidenceVotingPower: number;
};

export type NetworkMetrics = {
  unique_delegators: number | string;
  total_delegations: number | string;
  total_gov_action_proposals: number | string;
  total_drep_votes: number | string;
  total_registered_dreps: number | string;
  total_drep_distr: number | string;
  total_active_dreps: number | string;
  total_inactive_dreps: number | string;
  total_active_cip119_compliant_dreps: number | string;
  total_registered_direct_voters: number | string;
  no_of_committee_members: number | string;
  quorum_numerator: number | string;
  quorum_denominator: number | string;
};

export type GetNetworkMetricsResponse = {
  uniqueDelegators: number;
  totalDelegations: number;
  totalGovernanceActions: number;
  totalDRepVotes: number;
  totalRegisteredDReps: number;
  totalDRepDistr: number;
  totalActiveDReps: number;
  totalInactiveDReps: number;
  totalActiveCIP119CompliantDReps: number;
  totalRegisteredDirectVoters: number;
  noOfCommitteeMembers: number;
  quorumNumerator: number;
  quorumDenominator: number;
};
