import { Dispatch, SetStateAction, useEffect } from "react";
import { useLocation } from "react-router-dom";
import { Box } from "@mui/material";

import { useCardano } from "@context";
import {
  defaultEditDRepInfoValues,
  useEditDRepInfoForm,
  useGetDRepDetailsQuery,
  useTranslation,
} from "@hooks";
import { CenteredBoxBottomButtons, DRepDataForm } from "@molecules";
import { DRepData } from "@/models";

export const EditDRepForm = ({
  onClickCancel,
  setStep,
  loadUserData,
  setLoadUserData,
}: {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
  loadUserData: boolean;
  setLoadUserData: Dispatch<SetStateAction<boolean>>;
}) => {
  const { state } = useLocation();
  const { t } = useTranslation();
  const { dRepID } = useCardano();
  const { control, errors, isError, register, watch, reset } =
    useEditDRepInfoForm();
  const { dRep: yourselfDRep } = useGetDRepDetailsQuery(dRepID, {
    enabled: !state,
  });

  const onClickContinue = () => {
    setStep(2);
    setLoadUserData(false);
  };

  const isContinueButtonDisabled = !watch("givenName") || isError;

  useEffect(() => {
    if (loadUserData) {
      const data: DRepData = state ?? yourselfDRep;

      reset({
        ...data,
        objectives: data?.objectives ?? defaultEditDRepInfoValues.objectives,
        motivations: data?.motivations ?? defaultEditDRepInfoValues.motivations,
        qualifications:
          data?.qualifications ?? defaultEditDRepInfoValues.qualifications,
        paymentAddress:
          data?.paymentAddress ?? defaultEditDRepInfoValues.paymentAddress,
        image: data?.image ?? defaultEditDRepInfoValues.image,
        linkReferences:
          Array.isArray(data?.linkReferences) && data.linkReferences.length > 0
            ? data.linkReferences
            : defaultEditDRepInfoValues.linkReferences,
        identityReferences:
          Array.isArray(data?.identityReferences) && data.identityReferences.length > 0
            ? data.identityReferences
            : defaultEditDRepInfoValues.identityReferences,
      });
    }
  }, [yourselfDRep, loadUserData]);

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 8 }}>
      <DRepDataForm
        control={control}
        errors={errors}
        register={register}
        watch={watch}
      />
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        disableActionButton={isContinueButtonDisabled}
        onBackButton={onClickCancel}
        backButtonText={t("cancel")}
      />
    </Box>
  );
};
