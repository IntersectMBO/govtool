import { useState, useEffect } from "react";

import { useValidateMutation } from "@/hooks/mutations";
import { MetadataStandard, ProposalData, VotedProposal } from "@/models";
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
  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const { validateMetadata } = useValidateMutation();
  // Only the resolved metadata is kept here. The vote and the rest come from
  // the current props on every render, so a changed vote shows up.
  const [resolvedMetadata, setResolvedMetadata] = useState<
    Partial<Pick<ProposalData, "title" | "abstract" | "motivation" | "rationale">>
  >({});

  useEffect(() => {
    if (!votedProposal.proposal.url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status, metadata } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: votedProposal.proposal.url,
        hash: votedProposal.proposal.metadataHash,
      });

      if (metadata) {
        setResolvedMetadata(
          metadata as Pick<
            ProposalData,
            "title" | "abstract" | "motivation" | "rationale"
          >,
        );
      }
      setMetadataStatus(status);
      setIsValidating(false);
    };
    validate();
  }, [votedProposal.proposal.url]);

  return (
    <GovernanceVotedOnCard
      votedProposal={{
        ...votedProposal,
        proposal: { ...votedProposal.proposal, ...resolvedMetadata },
      }}
      inProgress={inProgress}
      isValidating={isValidating}
      metadataStatus={metadataStatus}
    />
  );
};
