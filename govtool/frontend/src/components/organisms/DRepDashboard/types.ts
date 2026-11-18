import type {
  DRepData,
  GovernanceActionRecord,
  VotedProposal,
  VoterInfo,
} from "@/models";

/** What every DRep dashboard tab reads, loaded once by DRepDashboard. */
export type DashboardData = {
  voter: VoterInfo;
  dRep?: DRepData;
  dRepLoading: boolean;
  /** The DRep credential hash, as the backend's DRep routes take it. */
  dRepID: string;
  stakeKey?: string;
  currentEpoch?: number;
  votes?: VotedProposal[];
  votesLoading: boolean;
  votesError: boolean;
  retryVotes: () => void;
  actions?: GovernanceActionRecord[];
  /** Records by `txHash#index`. */
  actionsByKey: Map<string, GovernanceActionRecord>;
  actionsLoading: boolean;
  actionsError: boolean;
  retryActions: () => void;
  registeredEpoch?: number;
  /** When the votes or records last arrived, epoch ms. */
  updatedAt: number;
  /** The DRep's public profile URL, for sharing. */
  profileLink?: string;
};
