import { useState, useEffect } from "react";

import { GovernanceActionCard } from "@molecules";
import { useValidateMutation } from "@/hooks/mutations";
import { MetadataStandard, ProposalData } from "@/models";

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
  // Only the resolved metadata is kept here. The rest comes from the current
  // props on every render, so a change such as `inProgress` clearing shows up.
  const [resolvedMetadata, setResolvedMetadata] = useState<
    Partial<Pick<ProposalData, "title" | "abstract" | "motivation" | "rationale">>
  >({});

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
  }, [props?.url]);

  return (
    <GovernanceActionCard
      {...props}
      {...resolvedMetadata}
      isValidating={isValidating}
      metadataStatus={metadataStatus}
    />
  );
};
