import { Dispatch, SetStateAction } from "react";
import { Box, Button } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { VoteContextWrapper } from "@organisms";
import { NodeObject } from "jsonld";

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
        {t("createGovernanceAction.storeDataTitle")}
      </Typography>
      <Spacer y={isMobile ? 4 : 6} />
      <Box sx={{ display: "flex", gap: 2 }}>
        <Button
          variant="outlined"
          onClick={handleStoreItMyself}
          sx={{ width: "100%" }}
        >
          {t("createGovernanceAction.storeDataYourself")}
        </Button>
        <Button
          variant="contained"
          onClick={handleLetGovToolStore}
          sx={{ width: "100%" }}
        >
          {t("createGovernanceAction.letGovToolStore")}
        </Button>
      </Box>
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </VoteContextWrapper>
  );
};
