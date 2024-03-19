import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  DashboardTopNav,
  DelegateTodRepStepOne,
  DelegateTodRepStepTwo,
  Footer,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export function DelegateTodRep() {
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
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav title={t("delegation.toDRep.title")} />
        <Box
          display="flex"
          height={isMobile ? "100%" : "auto"}
          flex={isMobile ? 1 : 0}
          justifyContent="center"
          mt={3}
        >
          {step === 1 && <DelegateTodRepStepOne setStep={setStep} />}
          {step === 2 && <DelegateTodRepStepTwo setStep={setStep} />}
        </Box>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
}
