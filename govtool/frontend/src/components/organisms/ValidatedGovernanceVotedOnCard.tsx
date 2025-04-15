import { useState, useRef, useEffect } from "react";

import { useValidateMutation } from "@/hooks/mutations";
import { MetadataStandard, VotedProposal } from "@/models";
import { GovernanceVotedOnCard } from "../molecules";

type Props = {
  votedProposal: VotedProposal;
  inProgress?: boolean;
};
export const ValidatedGovernanceVotedOnCard = ({
  votedProposal,
  inProgress,
}: Props) => {
  const [isValidating, setIsValidating] = useState(false);
  const metadataStatus = useRef<MetadataValidationStatus | undefined>();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    const validate = async () => {
      setIsValidating(true);

      const { status } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: votedProposal.proposal.url,
        hash: votedProposal.proposal.metadataHash,
      });

      metadataStatus.current = status;
      setIsValidating(false);
    };
    validate();
  }, []);

  return (
    <GovernanceVotedOnCard
      votedProposal={votedProposal}
      inProgress={inProgress}
      isValidating={isValidating}
      metadataStatus={metadataStatus.current}
    />
  );
};
