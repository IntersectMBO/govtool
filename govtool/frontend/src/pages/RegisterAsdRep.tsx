import { useEffect, useState } from "react";
import { Box } from "@mui/material";
import { FormProvider } from "react-hook-form";

import { Background } from "@atoms";
import { ICONS, PATHS } from "@consts";
import {
  DashboardTopNav,
  Footer,
  RegisterAsdRepStepOne,
  RegisterAsdRepStepTwo,
} from "@organisms";
import {
  useScreenDimension,
  useUrlAndHashFormController as useRegisterAsdRepFormController,
} from "@hooks";
import { useNavigate } from "react-router-dom";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();

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
      <Box display={"flex"} minHeight={"100vh"} flexDirection="column">
        <DashboardTopNav
          imageSRC={ICONS.appLogoIcon}
          imageWidth={isMobile ? undefined : 42}
          imageHeight={isMobile ? 24 : 35}
          title={"Register as a DRep"}
        />
        <Box
          display={"flex"}
          justifyContent={"center"}
          mt={isMobile ? 0 : 7}
          height={isMobile ? "100%" : "auto"}
          sx={{ marginTop: isMobile ? "97px" : "153px" }}
        >
          <FormProvider {...registerAsdRepFormMethods}>
            {step === 1 && <RegisterAsdRepStepOne setStep={setStep} />}
            {step === 2 && <RegisterAsdRepStepTwo setStep={setStep} />}
          </FormProvider>
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
