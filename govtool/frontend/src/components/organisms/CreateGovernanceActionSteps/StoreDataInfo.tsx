import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useCreateGovernanceActionForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import { ControlledField } from "@organisms";
import { openInNewTab } from "@utils";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StoreDataInfo = ({ setStep }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useCreateGovernanceActionForm();
  const { isMobile } = useScreenDimension();

  // TODO: change link when available
  const openLink = () => openInNewTab("https://docs.gov.tools");

  const isContinueDisabled = !watch("storeData");

  const onClickContinue = () => setStep(6);

  const onClickBack = () => setStep(4);

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.storeDataTitle")}
      </Typography>
      <Link
        data-testid="storing-information-link"
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
        data-testid="storing-information-checkbox"
        name="storeData"
        label={t("createGovernanceAction.storeDataCheckboxLabel")}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        disableActionButton={isContinueDisabled}
        onBackButton={onClickBack}
        backButtonText={t("cancel")}
      />
    </>
  );
};
