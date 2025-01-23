import { Box } from "@mui/material";

import { NewConstitutionAnchor, ProposalData } from "@/models";
import { useScreenDimension } from "@/hooks";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";

export const GovernanceActionNewConstitutionDetailsTabContent = ({
  details,
}: Pick<ProposalData, "details">) => {
  const { screenWidth } = useScreenDimension();
  return (
    <Box>
      <GovernanceActionCardElement
        isLinkButton
        label="New constitution link"
        text={(details?.anchor as NewConstitutionAnchor)?.url as string}
        dataTestId="new-constitution-url"
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
      <GovernanceActionCardElement
        isCopyButton
        label="New constitution hash"
        text={(details?.anchor as NewConstitutionAnchor)?.dataHash as string}
        dataTestId="new-constitution-data-hash"
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
    </Box>
  );
};
