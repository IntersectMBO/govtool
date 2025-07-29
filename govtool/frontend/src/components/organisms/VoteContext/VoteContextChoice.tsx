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
    <VoteContextWrapper hideAllBtn={true}>
        <Typography sx={{ textAlign: "center" , fontWeight : 500 , marginTop: isMobile ? 0 : 1 }} variant="headline4">
          {t("createGovernanceAction.storingOptionsForYourVoterRationale")}
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
        <Spacer y={isMobile ? 14 : 16} />
        <Typography sx={{ textAlign: "center" , fontWeight: 700 }} variant="body1">
          {t("createGovernanceAction.chooseDataStorageOption")}
        </Typography>
        <Spacer y={4}/>
        <Box sx={{ display: "flex", flexDirection: isMobile ? "column" : "row" ,  width: "100%", justifyContent: isMobile ? "none" : "space-between" , gap: isMobile ? "14px" : "0px" }}>
          <Button
            variant="outlined"
            onClick={handleLetGovToolStore}
            sx={{ width: isMobile ? "100%" : "259px", whiteSpace: "nowrap" , height:"48px" , fontWeight:"500" }}
          >
            {t("createGovernanceAction.govToolPinsDataToIPFS")}
          </Button>
          <Button
            variant="outlined"
            onClick={handleStoreItMyself}
            sx={{ width: isMobile ? "100%" :  "287px", whiteSpace: "nowrap" , height:"48px" , fontWeight:"500" }}
          >
            {t("createGovernanceAction.downloadAndStoreYourself")}
          </Button>
        </Box>
    </VoteContextWrapper>
  );
};
