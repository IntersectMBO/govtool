import { Dispatch, SetStateAction } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useCreateGovernanceActionForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { BgCard, ControlledField } from "@organisms";
import { openInNewTab } from "@utils";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StoreDataInfo = ({ setStep }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useCreateGovernanceActionForm();
  const { isMobile } = useScreenDimension();

  // TODO: change link when available
  const openLink = () => {
    openInNewTab("https://docs.sanchogov.tools");
  };

  const isContinueDisabled = !watch("storeData");

  const onClickContinue = () => {
    setStep(6);
  };

  const onClickBack = () => {
    setStep(4);
  };

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      isActionButtonDisabled={isContinueDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBack}
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
        name="storeData"
        label={t("createGovernanceAction.storeDataCheckboxLabel")}
      />
      <Spacer y={isMobile ? 4 : 12.5} />
      <Box display="flex" flex={1} />
    </BgCard>
  );
};
