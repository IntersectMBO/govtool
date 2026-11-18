import { useEffect, useState } from "react";
import { useNavigate } from "react-router";
import { FormProvider, useForm } from "react-hook-form";
import { CircularProgress } from "@mui/material";

import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  useTranslation,
  defaultRegisterAsDRepValues,
  useGetVoterInfo,
  useGetDRepDetailsQuery,
} from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import {
  DRepStorageInformation,
  DRepStoreDataInfo,
  RegisterAsDRepForm,
  ReRegisterIntro,
  RolesAndResponsibilities,
  WrongRouteInfo,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const RegisterAsdRep = () => {
  const { dRepID } = useCardano();
  const [step, setStep] = useState<number>(1);
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { closeModal, openModal } = useModal();
  const { voter } = useGetVoterInfo();
  const { dRep } = useGetDRepDetailsQuery(dRepID);

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
    // A returning DRep starts at "Register as a DRep again", then the form.
    if (voter?.wasRegisteredAsDRep) setStep(0);
  }, [voter?.wasRegisteredAsDRep]);

  const pageTitle = t("registration.becomeADRep");

  if (!voter)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle} hideBox>
        <CircularProgress />
      </CenteredBoxPageWrapper>
    );

  if (voter.isRegisteredAsDRep)
    return (
      <CenteredBoxPageWrapper pageTitle={pageTitle}>
        <WrongRouteInfo
          title={t(`registration.alreadyRegistered.title`)}
          description={t(`registration.alreadyRegistered.description`)}
          primaryButtonText={t("registration.alreadyRegistered.viewDetails")}
          onPrimaryButton={() => {
            if (dRep) {
              navigate(
                PATHS.dashboardDRepDirectoryDRep.replace(":dRepId", dRep.view),
                { state: { enteredFromWithinApp: true } },
              );
            }
          }}
        />
      </CenteredBoxPageWrapper>
    );

  return (
    <CenteredBoxPageWrapper
      pageTitle={pageTitle}
      onClickBackToDashboard={onClickBackToDashboard}
      stepper={step === 0 ? undefined : {
        // A returning DRep starts at the form; the steps count from there.
        steps: (voter?.wasRegisteredAsDRep
          ? []
          : [t("flow.steps.understand")]
        ).concat([
          t("flow.steps.profile"),
          t("flow.steps.prepareFile"),
          t("flow.steps.publish"),
        ]),
        current: voter?.wasRegisteredAsDRep ? step - 1 : step,
      }}
    >
      {step === 0 && (
        <ReRegisterIntro onCancel={onClickBackToDashboard} onContinue={() => setStep(2)} />
      )}
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
