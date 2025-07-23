import { useState, useEffect } from "react";

import { useValidateMutation } from "@/hooks/mutations";
import { MetadataStandard, ProposalData } from "@/models";
import { GovernanceActionCard } from "../molecules";

type ActionTypeProps = Omit<
  ProposalData,
  | "yesVotes"
  | "noVotes"
  | "abstainVotes"
  | "id"
  | "details"
  | "rationale"
  | "motivation"
> & {
  onClick?: () => void;
  inProgress?: boolean;
};
export const ValidatedGovernanceActionCard = (props: ActionTypeProps) => {
  const [isValidating, setIsValidating] = useState(false);
  const [metadataStatus, setMetadataStatus] = useState<
    MetadataValidationStatus | undefined
  >();
  const { validateMetadata } = useValidateMutation();
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    props as ProposalData,
  );

  useEffect(() => {
    if (!props?.url) return;

    const validate = async () => {
      setIsValidating(true);

      const { status, metadata } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: props?.url,
        hash: props?.metadataHash ?? "",
      });

      if (metadata) {
        setExtendedProposal(
          (prevProposal) =>
            ({
              ...(prevProposal || {}),
              ...(metadata as Pick<
                ProposalData,
                "title" | "abstract" | "motivation" | "rationale"
              >),
            } as ProposalData),
        );
      }

      setMetadataStatus(status);
      setIsValidating(false);
    };
    validate();
  }, [props?.url]);

  return (
    <GovernanceActionCard
      {...extendedProposal}
      isValidating={isValidating}
      metadataStatus={metadataStatus}
    />
  );
};
