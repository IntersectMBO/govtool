import { useEffect, useState } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { FormProvider, useForm } from "react-hook-form";
import { Box, CircularProgress } from "@mui/material";

import { Background } from "@atoms";
import { PATHS } from "@consts";
import { useModal } from "@context";
import {
  useScreenDimension,
  useTranslation,
  defaultEditDRepInfoValues,
  useGetVoterInfo,
} from "@hooks";
import { LinkWithIcon } from "@molecules";
import {
  DashboardTopNav,
  EditDRepStorageInformation,
  EditDRepStoreDataInfo,
  Footer,
  EditDRepForm,
} from "@organisms";
import { checkIsWalletConnected } from "@utils";

export const EditDRepMetadata = () => {
  const [step, setStep] = useState<number>(1);
  const [loadUserData, setLoadUserData] = useState(true);
  const { isMobile } = useScreenDimension();
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
    <Background isReverted>
      <Box
        sx={{ display: "flex", flexDirection: "column", minHeight: "100vh" }}
      >
        <DashboardTopNav title={t("editMetadata.pageTitle")} />
        <LinkWithIcon
          label={t("backToDashboard")}
          onClick={onClickBackToDashboard}
          sx={{
            mb: isMobile ? 0 : 1.5,
            ml: isMobile ? 2 : 5,
            mt: isMobile ? 3 : 1.5,
          }}
        />
        {!voter || !voter.isRegisteredAsDRep ? (
          <Box
            sx={{
              display: "flex",
              flex: 1,
              height: "100%",
              alignItems: "center",
              justifyContent: "center",
            }}
          >
            <CircularProgress />
          </Box>
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
        {/* FIXME: Footer should be on top of the layout.
        Should not be rerendered across the pages */}
        <Footer />
      </Box>
    </Background>
  );
};
