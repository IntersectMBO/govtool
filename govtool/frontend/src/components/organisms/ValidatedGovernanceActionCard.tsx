import { useState, useRef, useEffect } from "react";

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
  const metadataStatus = useRef<MetadataValidationStatus | undefined>();
  const { validateMetadata } = useValidateMutation();
  const [extendedProposal, setExtendedProposal] = useState<ProposalData>(
    props as ProposalData,
  );

  useEffect(() => {
    const validate = async () => {
      setIsValidating(true);

      const { status, metadata } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: props?.url ?? "",
        hash: props?.metadataHash ?? "",
      });

      metadataStatus.current = status;

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

      setIsValidating(false);
    };
    validate();
  }, [props?.url, props?.metadataHash]);

  return (
    <GovernanceActionCard
      {...extendedProposal}
      isValidating={isValidating}
      metadataStatus={metadataStatus.current}
    />
  );
};
