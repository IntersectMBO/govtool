import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { FormProvider, useForm } from "react-hook-form";
import { Box, CircularProgress } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useModal } from "@context";
import {
  useScreenDimension,
  useTranslation,
  defaultRegisterAsDRepValues,
  useGetVoterInfo,
} from "@hooks";
import { LinkWithIcon } from "@molecules";
import {
  DashboardTopNav,
  DRepStorageInformation,
  DRepStoreDataInfo,
  Footer,
  RegisterAsDRepForm,
  RolesAndResponsibilities,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(1);
  const { isMobile } = useScreenDimension();
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { closeModal, openModal } = useModal();
  const { voter } = useGetVoterInfo();

  const methods = useForm({
    mode: "onChange",
    defaultValues: defaultRegisterAsDRepValues,
  });

  const backToDashboard = () => {
    navigate(PATHS.dashboard);
    closeModal();
  };

  const onClickBackToDashboard = () =>
    openModal({
      type: "statusModal",
      state: {
        status: "warning",
        message: t("modals.registration.cancelDescription"),
        buttonText: t("modals.common.goToDashboard"),
        title: t("modals.registration.cancelTitle"),
        dataTestId: "cancel-registration-modal",
        onSubmit: backToDashboard,
      },
    });

  useEffect(() => {
    if (checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  useEffect(() => {
    if (voter?.wasRegisteredAsDRep) setStep(2);
  }, [voter?.wasRegisteredAsDRep]);

  return (
    <Background isReverted>
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav
          isVotingPowerHidden
          title={t("registration.becomeADRep")}
        />
        <LinkWithIcon
          label={t("backToDashboard")}
          onClick={onClickBackToDashboard}
          sx={{
            mb: isMobile ? 0 : 1.5,
            ml: isMobile ? 2 : 5,
            mt: isMobile ? 3 : 1.5,
          }}
        />
        {!voter ? (
          <Box
            sx={{
              alignItems: "center",
              display: "flex",
              flex: 1,
              height: "100vh",
              justifyContent: "center",
            }}
          >
            <CircularProgress />
          </Box>
        ) : (
          <>
            {step === 1 && !voter?.wasRegisteredAsDRep && (
              <RolesAndResponsibilities
                onClickCancel={onClickBackToDashboard}
                setStep={setStep}
              />
            )}
            <FormProvider {...methods}>
              {step === 2 && (
                <RegisterAsDRepForm
                  onClickCancel={onClickBackToDashboard}
                  setStep={setStep}
                  voter={voter}
                />
              )}
              {step === 3 && <DRepStoreDataInfo setStep={setStep} />}
              {step === 4 && <DRepStorageInformation setStep={setStep} />}
            </FormProvider>
          </>
        )}
        <Footer />
      </Box>
    </Background>
  );
};
