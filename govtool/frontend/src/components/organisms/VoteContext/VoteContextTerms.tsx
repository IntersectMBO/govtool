import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { useScreenDimension, useTranslation, useVoteContextForm } from "@hooks";
import { ControlledField, VoteContextWrapper } from "@organisms";
import { openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
  onCancel: () => void;
};

export const VoteContextTerms = ({ setStep, onCancel }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useVoteContextForm();
  const { isMobile } = useScreenDimension();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const isContinueDisabled = !watch("terms");

  return (
    <VoteContextWrapper
      onContinue={() => setStep(4)}
      isContinueDisabled={isContinueDisabled}
      onCancel={onCancel}
      isVoteWithMetadata
    >
      <Typography sx={{ textAlign: "center" , fontWeight: 500 , fontSize : 28 }} variant="headline4">
        {t("createGovernanceAction.storeDataTitle")}
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
        {t("createGovernanceAction.storeDataLink")}
      </Link>
      <ControlledField.Checkbox
        {...{ control, errors }}
        name="terms"
        label={t("createGovernanceAction.storeDataCheckboxLabel")}
        layoutStyles={{
          display: "flex",
          justifyContent: "center",
        }}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </VoteContextWrapper>
  );
};
