import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { FormProvider } from "react-hook-form";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import {
  useScreenDimension,
  useUrlAndHashFormController as useRegisterAsdRepFormController,
  useTranslation,
} from "@hooks";
import {
  DashboardTopNav,
  Footer,
  RegisterAsdRepStepOne,
  RegisterAsdRepStepThree,
  RegisterAsdRepStepTwo,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  const registerAsdRepFormMethods = useRegisterAsdRepFormController();

  useEffect(() => {
    if (checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  return (
    <Background isReverted>
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav title={t("registration.becomeADRep")} />
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
