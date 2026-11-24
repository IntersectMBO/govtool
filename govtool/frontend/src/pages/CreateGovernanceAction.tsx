import { useEffect, useState } from "react";
import { useNavigate } from "react-router";
import { FormProvider, useForm } from "react-hook-form";

import { PATHS } from "@consts";
import { useModal } from "@context";
import { defaulCreateGovernanceActionValues, useTranslation } from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import {
  ChooseGovernanceActionType,
  CreateGovernanceActionForm,
  ReviewCreatedGovernanceAction,
  StorageInformation,
  StoreDataInfo,
  WhatGovernanceActionIsAbout,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const CreateGovernanceAction = () => {
  const [step, setStep] = useState(1);
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { closeModal, openModal } = useModal();

  const methods = useForm({
    mode: "onChange",
    defaultValues: defaulCreateGovernanceActionValues,
  });

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
    }
  }, []);

  const backToDashboard = () => {
    navigate(PATHS.dashboard);
    closeModal();
  };

  const onClickBackToDashboard = () =>
    openModal({
      type: "statusModal",
      state: {
        status: "warning",
        message: t("createGA.modals.cancelBody"),
        buttonText: t("createGA.modals.cancelConfirm"),
        cancelText: t("createGA.modals.cancelBack"),
        title: t("createGA.modals.cancelTitle"),
        dataTestId: "cancel-governance-action-creation-modal",
        onSubmit: backToDashboard,
      },
    });

  return (
    <CenteredBoxPageWrapper
      pageTitle={t("createGovernanceAction.title")}
      frameless
      onClickBackToDashboard={onClickBackToDashboard}
      stepper={{
        steps: [
          t("createGA.steps.understand"),
          t("createGA.steps.type"),
          t("createGA.steps.details"),
          t("createGA.steps.review"),
          t("createGA.steps.prepareFile"),
          t("createGA.steps.confirm"),
        ],
        current: step,
      }}
    >
      <FormProvider {...methods}>
        {step === 1 && (
          <WhatGovernanceActionIsAbout
            onClickCancel={onClickBackToDashboard}
            setStep={setStep}
          />
        )}
        {step === 2 && <ChooseGovernanceActionType setStep={setStep} />}
        {step === 3 && <CreateGovernanceActionForm setStep={setStep} />}
        {step === 4 && <ReviewCreatedGovernanceAction setStep={setStep} />}
        {step === 5 && <StoreDataInfo setStep={setStep} />}
        {step === 6 && <StorageInformation setStep={setStep} />}
      </FormProvider>
    </CenteredBoxPageWrapper>
  );
};
