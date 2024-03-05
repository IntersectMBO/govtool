import { Dispatch, SetStateAction, useCallback } from "react";
import { Trans } from "react-i18next";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  correctAdaFormat,
  getItemFromLocalStorage,
  PROTOCOL_PARAMS_KEY,
} from "@utils";

import { BgCard } from ".";

type WhatGovernanceActionIsAboutProps = {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
};

export const WhatGovernanceActionIsAbout = ({
  setStep,
  onClickCancel,
}: WhatGovernanceActionIsAboutProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const deposit = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  const onClickContinue = useCallback(() => setStep(2), []);

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("cancel")}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickCancel}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.creatingAGovernanceAction")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          pb: isMobile ? 6 : 4,
          pt: 4,
          textAlign: "center",
          whiteSpace: "pre-line",
        }}
        variant="body1"
      >
        <Trans
          i18nKey="createGovernanceAction.creatingAGovernanceActionDescription"
          values={{
            deposit: correctAdaFormat(deposit.gov_action_deposit),
          }}
        />
      </Typography>
    </BgCard>
  );
};
