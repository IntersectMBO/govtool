import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { FormProvider, useForm } from "react-hook-form";
import { Box, CircularProgress } from "@mui/material";

import { PATHS } from "@consts";
import { useModal } from "@context";
import {
  useTranslation,
  defaultRegisterAsDRepValues,
  useGetVoterInfo,
} from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import {
  DRepStorageInformation,
  DRepStoreDataInfo,
  RegisterAsDRepForm,
  RolesAndResponsibilities,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const RegisterAsdRep = () => {
  const [step, setStep] = useState<number>(1);
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
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  useEffect(() => {
    if (voter?.wasRegisteredAsDRep) setStep(2);
  }, [voter?.wasRegisteredAsDRep]);

  const pageTitle = t("registration.becomeADRep");

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox>
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
      </CenteredBoxPageWrapper>
    );

  return (
    <CenteredBoxPageWrapper
      pageTitle={pageTitle}
      onClickBackToDashboard={onClickBackToDashboard}
    >
      {step === 1 && !voter?.wasRegisteredAsDRep && (
        <RolesAndResponsibilities setStep={setStep} />
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
    </CenteredBoxPageWrapper>
  );
};
