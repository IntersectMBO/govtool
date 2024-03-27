import { Box } from "@mui/material";

import { ExternalModalButton } from "@atoms";
import {
  GovernanceActionCardElement,
  GovernanceActionDetailsCardLinks,
  DataMissingInfoBox,
  GovernanceActionDetailsCardHeader,
  GovernanceActionsDatesBox,
  GovernanceActionDetailsCardOnChainData,
} from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { getProposalTypeNoEmptySpaces } from "@utils";

const mockedLongDescription =
  "I am the Cardano crusader carving his path in the blockchain battleground. With a mind sharper than a Ledger Nano X, this fearless crypto connoisseur fearlessly navigates the volatile seas of Cardano, turning code into currency. Armed with a keyboard and a heart pumping with blockchain beats, Mister Big Bad fearlessly champions decentralization, smart contracts, and the Cardano community. His Twitter feed is a mix of market analysis that rivals CNBC and memes that could break the internet.";

type GovernanceActionDetailsCardDataProps = {
  type: string;
  govActionId: string;
  createdDate: string;
  createdEpochNo: number;
  expiryDate: string;
  expiryEpochNo: number;
  details?: ActionDetailsType;
  url: string;
  title: string | null;
  about: string | null;
  motivation: string | null;
  rationale: string | null;
  isDataMissing: boolean;
  isOneColumn: boolean;
  isDashboard?: boolean;
};

export const GovernanceActionDetailsCardData = ({
  type,
  govActionId,
  createdDate,
  createdEpochNo,
  expiryDate,
  expiryEpochNo,
  details,
  url,
  title,
  about,
  motivation,
  rationale,
  isDataMissing,
  isOneColumn,
  isDashboard,
}: GovernanceActionDetailsCardDataProps) => {
  const { t } = useTranslation();
  const { screenWidth } = useScreenDimension();

  const isModifiedPadding =
    (isDashboard && screenWidth < 1168) ?? screenWidth < 900;

  return (
    <Box
      sx={{
        borderRadius: isOneColumn ? "20px 20px 0 0" : "20px 0 0 20px",
        bgcolor: "rgba(255, 255, 255, 0.30)",
        p: `40px ${isModifiedPadding ? "24px" : "40px"}`,
        overflow: "hidden",
      }}
    >
      <GovernanceActionDetailsCardHeader
        // TODO: Remove "Fund our project" when title is implemented everywhere
        title={title ?? "Fund our project"}
        // TODO: Modify props regarding missing data
        // (e.g. title, description) when validation is done
        isDataMissing={isDataMissing}
      />
      {isDataMissing && <DataMissingInfoBox />}
      <GovernanceActionCardElement
        label={t("govActions.governanceActionType")}
        text={type}
        textVariant="pill"
        dataTestId={`${getProposalTypeNoEmptySpaces(type)}-type`}
      />
      <GovernanceActionsDatesBox
        createdDate={createdDate}
        expiryDate={expiryDate}
        expiryEpochNo={expiryEpochNo}
        createdEpochNo={createdEpochNo}
      />
      {isDataMissing && (
        <ExternalModalButton
          url={url}
          label={t("govActions.seeExternalData")}
        />
      )}
      <GovernanceActionCardElement
        label={t("govActions.governanceActionId")}
        text={govActionId}
        isCopyButton
        dataTestId={`${govActionId}-id`}
      />
      <GovernanceActionCardElement
        label={t("govActions.about")}
        // TODO: Remove mock when possible
        text={about ?? mockedLongDescription}
        textVariant="longText"
        dataTestId="about"
      />
      <GovernanceActionCardElement
        label={t("govActions.motivation")}
        // TODO: Remove mock when possible
        text={motivation ?? mockedLongDescription}
        textVariant="longText"
        dataTestId="motivation"
      />
      <GovernanceActionCardElement
        label={t("govActions.rationale")}
        text={rationale ?? mockedLongDescription}
        textVariant="longText"
        dataTestId="rationale"
      />
      {details && Object.keys(details).length !== 0 && (
        <GovernanceActionDetailsCardOnChainData data={details} />
      )}
      <GovernanceActionDetailsCardLinks />
    </Box>
  );
};
