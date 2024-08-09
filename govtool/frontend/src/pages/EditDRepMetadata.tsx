import { useEffect, useState } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { FormProvider, useForm } from "react-hook-form";
import { CircularProgress } from "@mui/material";

import { PATHS } from "@consts";
import { useModal } from "@context";
import {
  useTranslation,
  defaultEditDRepInfoValues,
  useGetVoterInfo,
} from "@hooks";
import { CenteredBoxPageWrapper } from "@molecules";
import {
  EditDRepStorageInformation,
  EditDRepStoreDataInfo,
  EditDRepForm,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const EditDRepMetadata = () => {
  const [step, setStep] = useState<number>(1);
  const [loadUserData, setLoadUserData] = useState(true);
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { closeModal, openModal } = useModal();
  const { state } = useLocation();
  const { voter } = useGetVoterInfo({ enabled: !state });

  const methods = useForm({
    mode: "onChange",
    defaultValues: defaultEditDRepInfoValues,
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
        dataTestId: "cancel-edit-drep-info-modal",
        onSubmit: backToDashboard,
      },
    });

  useEffect(() => {
    if (!checkIsWalletConnected()) {
      navigate(PATHS.home);
      return;
    }
    if (voter && !voter?.isRegisteredAsDRep) navigate(PATHS.dashboard);
  }, [voter]);

  return (
    <CenteredBoxPageWrapper
      pageTitle={t("editMetadata.pageTitle")}
      onClickBackToDashboard={onClickBackToDashboard}
      showVotingPower
    >
      {!voter || !voter.isRegisteredAsDRep ? (
        <CircularProgress />
      ) : (
        <FormProvider {...methods}>
          {step === 1 && (
            <EditDRepForm
              onClickCancel={onClickBackToDashboard}
              setStep={setStep}
              loadUserData={loadUserData}
              setLoadUserData={setLoadUserData}
            />
          )}
          {step === 2 && <EditDRepStoreDataInfo setStep={setStep} />}
          {step === 3 && <EditDRepStorageInformation setStep={setStep} />}
        </FormProvider>
      )}
    </CenteredBoxPageWrapper>
  );
};
