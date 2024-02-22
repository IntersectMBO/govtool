import { Dispatch, SetStateAction, useCallback } from "react";
import { Trans } from "react-i18next";
import { Link } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  correctAdaFormat,
  getItemFromLocalStorage,
  openInNewTab,
  PROTOCOL_PARAMS_KEY,
} from "@utils";

import { BgCard } from ".";

export const RegisterAsdRepStepOne = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const deposit = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  const onClickContinue = useCallback(() => setStep(2), []);

  const openLearMoreAboutDrep = useCallback(
    () => openInNewTab("https://sancho.network/roles/drep"),
    []
  );

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      onClickActionButton={onClickContinue}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.rolesAndResponsibilitiesTitle")}
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
          components={[
            <Link
              key="1"
              onClick={openLearMoreAboutDrep}
              sx={{ cursor: "pointer" }}
            />,
          ]}
          i18nKey={"registration.rolesAndResponsibilitiesDescription"}
          values={{ deposit: correctAdaFormat(deposit.drep_deposit) }}
        />
      </Typography>
    </BgCard>
  );
};
