import { Dispatch, SetStateAction } from "react";
import { Box, Button, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { VoteContextWrapper } from "@organisms";
import { NodeObject } from "jsonld";
import { openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";

type VoteContextChoiceProps = {
  setStep: Dispatch<SetStateAction<number>>;
  setStoreDataYourself: Dispatch<SetStateAction<boolean>>;
  setJsonldContent: Dispatch<SetStateAction<NodeObject | null>>;
  setMetadataHash: Dispatch<SetStateAction<string | null>>;
  generateMetadata: () => Promise<{ jsonld: NodeObject; jsonHash: string }>;
  onCancel: () => void;
};

export const VoteContextChoice = ({
  setStep,
  setStoreDataYourself,
  setJsonldContent,
  setMetadataHash,
  generateMetadata,
  onCancel,
}: VoteContextChoiceProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const handleStoreItMyself = () => {
    setStoreDataYourself(true);
    setStep(3);
  };

  const handleLetGovToolStore = async () => {
    setStoreDataYourself(false);
    const { jsonld, jsonHash } = await generateMetadata();
    setJsonldContent(jsonld);
    setMetadataHash(jsonHash);
    setStep(3);
  };

  return (
    <VoteContextWrapper onCancel={onCancel} showContinueButton={false}>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.storeAndMaintainDataTitle")}
      </Typography>
      <Link
        onClick={openLink}
        sx={{
          cursor: "pointer",
          fontSize: 16,
          fontWeight: 500,
          fontFamily: "Poppins",
          my: 4,
          textAlign: "center",
          textDecoration: "none",
        }}
      >
        {t("createGovernanceAction.learnMoreAboutStoringInformation")}
      </Link>
      <Typography fontWeight={400} sx={{ textAlign: "center" }} variant="body1">
        {t("createGovernanceAction.govToolProvidesOptions")}
      </Typography>
      <Box sx={{ my: 4 }}>
        <ul>
          <li>
            <Typography variant="body1">
              {t("createGovernanceAction.govToolCanPinToIPFS")}
            </Typography>
          </li>
          <li>
            <Typography variant="body1">
              {t("createGovernanceAction.storeYourselfInRepo")}
            </Typography>
          </li>
        </ul>
      </Box>
      <Typography sx={{ textAlign: "center" }} variant="body1">
        {t("createGovernanceAction.chooseDataStorageOption")}
      </Typography>
      <Spacer y={isMobile ? 4 : 6} />
      <Box sx={{ display: "flex", gap: 2, width: "100%", justifyContent: "center" }}>
        <Button
          variant="outlined"
          onClick={handleLetGovToolStore}
          sx={{ width: "250px", whiteSpace: "nowrap" }}
        >
          {t("createGovernanceAction.govToolPinsDataToIPFS")}
        </Button>
        <Button
          variant="contained"
          onClick={handleStoreItMyself}
          sx={{ width: "250px", whiteSpace: "nowrap" }}
        >
          {t("createGovernanceAction.downloadAndStoreYourself")}
        </Button>
      </Box>
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </VoteContextWrapper>
  );
};
