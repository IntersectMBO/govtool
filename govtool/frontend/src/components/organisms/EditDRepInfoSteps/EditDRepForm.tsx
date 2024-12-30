import { Dispatch, SetStateAction, useEffect } from "react";
import { useLocation } from "react-router-dom";
import { Box } from "@mui/material";

import { useCardano } from "@context";
import {
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
      const groupedReferences = data?.references?.reduce<
        Record<string, Reference[]>
      >((acc, reference) => {
        const type = reference["@type"];
        if (!acc[type]) {
          acc[type] = [];
        }
        acc[type].push(reference);
        return acc;
      }, {});
      reset({
        ...data,
        objectives: data?.objectives ?? "",
        motivations: data?.motivations ?? "",
        qualifications: data?.qualifications ?? "",
        paymentAddress: data?.paymentAddress ?? "",
        linkReferences: groupedReferences?.Link ?? [getEmptyReference("Link")],
        identityReferences: groupedReferences?.Identity ?? [
          getEmptyReference("Identity"),
        ],
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

const getEmptyReference = (type: "Link" | "Identity") => ({
  "@type": type,
  uri: "",
  label: "",
});
