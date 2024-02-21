import { useEffect, useState } from "react";
import { FormProvider, useForm } from "react-hook-form";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { ICONS, PATHS } from "@consts";
import {
  DashboardTopNav,
  Footer,
  RegisterAsdRepStepOne,
  RegisterAsdRepStepThree,
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
  const [step, setStep] = useState<number>(1);
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
          {step === 3 && <RegisterAsdRepStepThree setStep={setStep} />}
        </FormProvider>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
