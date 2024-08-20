import { Dispatch, SetStateAction } from "react";
import { Trans } from "react-i18next";
import { Link } from "@mui/material";

import { Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import {
  correctAdaFormat,
  getItemFromLocalStorage,
  openInNewTab,
  PROTOCOL_PARAMS_KEY,
} from "@utils";

export const RolesAndResponsibilities = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  const deposit = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

  const onClickContinue = () => setStep(2);

  const openLearnMoreAboutDrep = () =>
    openInNewTab("https://sancho.network/roles/drep");

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("registration.rolesAndResponsibilitiesTitle")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{
          pb: isMobile ? 4 : 6,
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
              onClick={openLearnMoreAboutDrep}
              sx={{ cursor: "pointer" }}
            />,
          ]}
          i18nKey="registration.rolesAndResponsibilitiesDescription"
          values={{ deposit: correctAdaFormat(deposit.drep_deposit) }}
        />
      </Typography>
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        backButtonText={t("cancel")}
      />
    </>
  );
};
