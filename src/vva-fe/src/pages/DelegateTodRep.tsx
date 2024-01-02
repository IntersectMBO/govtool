import { useEffect, useState } from "react";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { ICONS, PATHS } from "@consts";
import {
  DashboardTopNav,
  DelegateTodRepStepOne,
  DelegateTodRepStepTwo,
  Footer,
} from "@organisms";
import { useScreenDimension } from "@hooks";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@/utils/localStorage";
import { useNavigate } from "react-router-dom";

export const DelegateTodRep = () => {
  const [step, setStep] = useState(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();

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
          title={"Delegate to DRep"}
        />
        <Box
          display={"flex"}
          height={isMobile ? "100%" : "auto"}
          flex={isMobile ? 1 : 0}
          justifyContent={"center"}
          sx={{ marginTop: isMobile ? "97px" : "153px" }}
        >
          {step === 1 && <DelegateTodRepStepOne setStep={setStep} />}
          {step === 2 && <DelegateTodRepStepTwo setStep={setStep} />}
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
