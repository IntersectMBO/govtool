import { useAppContext } from "@context";
import { getFullGovActionId, getGovActionPath } from "@utils";
import { GovernanceActionCard } from "./GovernanceActionCard";
import {
  MetadataIssue,
  MetadataValidationStatus,
  VotedProposal,
} from "@/models";

type Props = {
  votedProposal: VotedProposal;
  inProgress?: boolean;
  isValidating?: boolean;
  metadataStatus?: MetadataValidationStatus;
  metadataIssues?: MetadataIssue[];
};

/** A governance action the connected voter voted on, with their vote. */
export const GovernanceVotedOnCard = ({
  votedProposal,
  inProgress,
  isValidating,
  metadataStatus,
  metadataIssues,
}: Props) => {
  const { proposal, vote } = votedProposal;
  const { cExplorerBaseUrl } = useAppContext();
  const govActionId = getFullGovActionId(proposal.txHash, proposal.index);

  return (
    <GovernanceActionCard
      {...proposal}
      inProgress={inProgress}
      isValidating={isValidating}
      link={{
        dataTestId: `govaction-${govActionId}-change-your-vote`,
        state: { proposal, vote },
        to: getGovActionPath(proposal.txHash, proposal.index),
      }}
      metadataIssues={metadataIssues}
      metadataStatus={metadataStatus}
      myVote={vote?.vote}
      myVoteTxUrl={vote?.txHash ? `${cExplorerBaseUrl}/tx/${vote.txHash}` : undefined}
    />
  );
};
