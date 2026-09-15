import { Box } from "@mui/material";

import { NewConstitutionAnchor, ProposalData } from "@/models";
import { useScreenDimension, useTranslation } from "@/hooks";

import { GovernanceActionCardElement } from "./GovernanceActionCardElement";

export const GovernanceActionNewConstitutionDetailsTabContent = ({
  details,
}: Pick<ProposalData, "details">) => {
  const { screenWidth } = useScreenDimension();
  const { t } = useTranslation();
  return (
    <Box>
      <GovernanceActionCardElement
        isLinkButton
        label={t("govActions.newConstitution.url")}
        text={(details?.anchor as NewConstitutionAnchor)?.url as string}
        dataTestId="new-constitution-url"
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
      <GovernanceActionCardElement
        isCopyButton
        label={t("govActions.newConstitution.hash")}
        text={(details?.anchor as NewConstitutionAnchor)?.dataHash as string}
        dataTestId="new-constitution-data-hash"
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
      <GovernanceActionCardElement
        isCopyButton
        label={t("govActions.newConstitution.scriptHash")}
        text={details?.script as string}
        dataTestId="new-constitution-script-hash"
        textVariant={screenWidth > 1600 ? "longText" : "oneLine"}
      />
    </Box>
  );
};
