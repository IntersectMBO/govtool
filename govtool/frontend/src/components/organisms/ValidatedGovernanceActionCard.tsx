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
export const ValidatedGovernanceActionCard = ({
  url,
  metadataHash,
  ...props
}: ActionTypeProps) => {
  const [isValidating, setIsValidating] = useState(false);
  const metadataStatus = useRef<MetadataValidationStatus | undefined>();
  const { validateMetadata } = useValidateMutation();

  useEffect(() => {
    const validate = async () => {
      setIsValidating(true);

      const { status } = await validateMetadata({
        standard: MetadataStandard.CIP108,
        url: url ?? "",
        hash: metadataHash ?? "",
      });

      metadataStatus.current = status;
      setIsValidating(false);
    };
    validate();
  }, []);

  return (
    <GovernanceActionCard
      {...props}
      url={url}
      metadataHash={metadataHash}
      isValidating={isValidating}
      metadataStatus={metadataStatus.current}
    />
  );
};
