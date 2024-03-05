import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { FormProvider, useForm } from "react-hook-form";
import { Box } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useModal } from "@context";
import {
  defaulCreateGovernanceActionValues,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { BackToButton } from "@molecules";
import {
  ChooseGovernanceActionType,
  CreateGovernanceActionForm,
  DashboardTopNav,
  Footer,
  WhatGovernanceActionIsAbout,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const CreateGovernanceAction = () => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { closeModal, openModal } = useModal();
  const [step, setStep] = useState(1);

  const methods = useForm({
    mode: "onBlur",
    defaultValues: defaulCreateGovernanceActionValues,
  });

  useEffect(() => {
    if (checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const onClickBackToDashboard = () =>
    openModal({
      type: "statusModal",
      state: {
        status: "warning",
        message: t("modals.createGovernanceAction.cancelModalDescription"),
        buttonText: t("modals.common.goToDashboard"),
        title: t("modals.createGovernanceAction.cancelModalTitle"),
        dataTestId: "cancel-governance-action-creation-modal",
        onSubmit: backToDashboard,
      },
    });

  const backToDashboard = () => {
    navigate(PATHS.dashboard);
    closeModal();
  };

  return (
    <Background isReverted>
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav title={t("createGovernanceAction.title")} />
        <BackToButton
          label={t("backToDashboard")}
          onClick={onClickBackToDashboard}
          sx={{
            mb: isMobile ? 0 : 1.5,
            ml: isMobile ? 2 : 5,
            mt: isMobile ? 3 : 1.5,
          }}
        />
        <FormProvider {...methods}>
          {step === 1 && (
            <WhatGovernanceActionIsAbout
              onClickCancel={onClickBackToDashboard}
              setStep={setStep}
            />
          )}
          {step === 2 && (
            <ChooseGovernanceActionType
              onClickCancel={onClickBackToDashboard}
              setStep={setStep}
            />
          )}
          {step === 3 && <CreateGovernanceActionForm setStep={setStep} />}
        </FormProvider>
        {isMobile && <Footer />}
      </Box>
    </Background>
  );
};
