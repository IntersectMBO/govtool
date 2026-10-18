import { useCallback, Dispatch, SetStateAction } from "react";
import { Trans } from "react-i18next";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import { useAppContext } from "@context";
import { correctVoteAdaFormat } from "@utils";

type WhatGovernanceActionIsAboutProps = {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
};

export const WhatGovernanceActionIsAbout = ({
  onClickCancel,
  setStep,
}: WhatGovernanceActionIsAboutProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const { epochParams } = useAppContext();

  const onClickContinue = useCallback(() => setStep(2), []);

  return (
    <>
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
            deposit: correctVoteAdaFormat(
              epochParams?.gov_action_deposit ?? undefined,
            ),
          }}
        />
      </Typography>
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        onBackButton={onClickCancel}
        backButtonText={t("cancel")}
      />
    </>
  );
};
