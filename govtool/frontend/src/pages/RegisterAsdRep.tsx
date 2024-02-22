import { useEffect, useState } from "react";
import { Box } from "@mui/material";
import { FormProvider } from "react-hook-form";

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
  useScreenDimension,
  useUrlAndHashFormController as useRegisterAsdRepFormController,
  useTranslation,
} from "@hooks";
import { useNavigate } from "react-router-dom";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const registerAsdRepFormMethods = useRegisterAsdRepFormController();

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
        <FormProvider {...registerAsdRepFormMethods}>
          {step === 1 && <RegisterAsdRepStepOne setStep={setStep} />}
          {step === 2 && <RegisterAsdRepStepTwo setStep={setStep} />}
          {step === 3 && <RegisterAsdRepStepThree setStep={setStep} />}
        </FormProvider>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
