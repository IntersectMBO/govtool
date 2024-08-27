import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import { useScreenDimension, useTranslation, useVoteContextForm } from "@hooks";
import { ControlledField, VoteContextWrapper } from "@organisms";
import { openInNewTab } from "@utils";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
  onCancel: () => void;
};

export const VoteContextTerms = ({ setStep, onCancel }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useVoteContextForm();
  const { isMobile } = useScreenDimension();

  const openLink = () =>
    openInNewTab(
      "https://docs.gov.tools/using-govtool/govtool-functions/storing-information-offline",
    );

  const isContinueDisabled = !watch("terms");

  return (
    <VoteContextWrapper
      onContinue={() => setStep(3)}
      isContinueDisabled={isContinueDisabled}
      onCancel={onCancel}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
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
