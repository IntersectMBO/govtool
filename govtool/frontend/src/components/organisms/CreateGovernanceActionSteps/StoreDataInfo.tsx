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
import { LINKS } from "@/consts/links";
import { FONT_FAMILY } from "@/consts/designTokens";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StoreDataInfo = ({ setStep }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useCreateGovernanceActionForm();
  const { isMobile } = useScreenDimension();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const isContinueDisabled = !watch("storeData");

  const onClickContinue = () => setStep(6);

  const onClickBack = () => setStep(4);

  return (
    <>
      <Typography component="h1" variant="headline4">
        {t("createGovernanceAction.storeDataTitle")}
      </Typography>
      <Link
        data-testid="storing-information-link"
        onClick={openLink}
        sx={{
          cursor: "pointer",
          fontSize: 16,
          fontWeight: 500,
          fontFamily: FONT_FAMILY.sans,
          my: 4,
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
