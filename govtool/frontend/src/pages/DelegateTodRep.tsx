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
import { useScreenDimension, useTranslation } from "@hooks";
import { useNavigate } from "react-router-dom";
import { checkIsWalletConnected } from "@/utils";

export const DelegateTodRep = () => {
  const [step, setStep] = useState(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();

  useEffect(() => {
    if (checkIsWalletConnected()) {
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
          title={t("delegation.toDRep.title")}
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
