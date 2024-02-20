import { useEffect, useState } from "react";
import { Box } from "@mui/material";
import { FormProvider, useForm } from "react-hook-form";

import { Background } from "@atoms";
import { ICONS, PATHS } from "@consts";
import {
  DashboardTopNav,
  Footer,
  RegisterAsdRepStepOne,
  RegisterAsdRepStepTwo,
} from "@organisms";
import {
  defaulDRepRegistrationtValues,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { useNavigate } from "react-router-dom";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(2);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const methods = useForm({
    mode: "onBlur",
    defaultValues: defaulDRepRegistrationtValues,
  });

  useEffect(() => {
    if (
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`) ||
      !getItemFromLocalStorage(`${WALLET_LS_KEY}_name`)
    ) {
      navigate(PATHS.home);
    }
  }, []);

  return (
    <Background isReverted>
      <Box display="flex" flexDirection="column" minHeight="100vh">
        <DashboardTopNav
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          imageHeight={isMobile ? 24 : 35}
          title={t("registration.becomeADRep")}
        />
        <FormProvider {...methods}>
          {step === 1 && <RegisterAsdRepStepOne setStep={setStep} />}
          {step === 2 && <RegisterAsdRepStepTwo setStep={setStep} />}
        </FormProvider>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
