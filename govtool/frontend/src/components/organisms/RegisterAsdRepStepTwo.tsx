import { Dispatch, SetStateAction, useCallback } from "react";
import { Box, Link } from "@mui/material";

import { Spacer, Typography } from "@atoms";
import {
  useScreenDimension,
  useRegisterAsdRepFormContext,
  useTranslation,
} from "@hooks";
import { openInNewTab } from "@utils";

import { BgCard, ControlledField } from ".";

interface Props {
  setStep: Dispatch<SetStateAction<number>>;
}

export const RegisterAsdRepStepTwo = ({ setStep }: Props) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors,isContinueButtonDisabled, isSkipButton  } = useRegisterAsdRepFormContext();

  const onClickContinue = useCallback(() => setStep(3), []);

  const onClickBackButton = useCallback(() => setStep(1), []);

  return (
    <BgCard
      actionButtonLabel={isSkipButton ? t("skip") : t("continue")}
      onClickActionButton={onClickContinue}
      isActionButtonDisabled={isContinueButtonDisabled}
      onClickBackButton={onClickBackButton}
    >
      <Typography
        color="accentOrange"
        sx={{ letterSpacing: 1.5, textAlign: "center" }}
        variant="body1"
      >
        {t("registration.optional")}
      </Typography>
      <Typography sx={{ mt: 1, textAlign: "center" }} variant="headline4">
        {t("registration.addInformationTitle")}
      </Typography>
      <Typography
        fontWeight={400}
        sx={{ mb: 7, mt: 3, textAlign: "center" }}
        variant="body1"
      >
        {t("registration.addInformationDescription")}
      </Typography>
      <Box display="flex" flexDirection="column" alignItems="center">
        <ControlledField.Input
          {...{ control, errors }}
          dataTestId="url-input"
          layoutStyles={{ width: isMobile ? "100%" : "70%" }}
          name="url"
          placeholder={t("forms.urlWithInfoPlaceholder")}
        />
        <Spacer y={6} />
        <ControlledField.Input
          {...{ control, errors }}
          dataTestId="hash-input"
          layoutStyles={{ width: isMobile ? "100%" : "70%" }}
          name="hash"
          placeholder={t("forms.hashPlaceholder")}
        />
        <Link
          data-testid={"how-to-create-link"}
          onClick={() =>
            openInNewTab(
              "https://docs.sanchogov.tools/faqs/how-to-create-a-metadata-anchor"
            )
          }
          alignSelf={"center"}
          my={5}
          sx={{ cursor: "pointer" }}
        >
          <Typography fontWeight={500} color="primary" variant="body1">
            {t("forms.howCreateUrlAndHash")}
          </Typography>
        </Link>
      </Box>
    </BgCard>
  );
};
