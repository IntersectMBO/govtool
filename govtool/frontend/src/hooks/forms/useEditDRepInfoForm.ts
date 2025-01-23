import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import * as Sentry from "@sentry/react";
import { NodeObject } from "jsonld";

import { DREP_CONTEXT, PATHS, storageInformationErrorModals } from "@consts";
import { useCardano, useModal, useAppContext } from "@context";
import { downloadJson, generateJsonld, generateMetadataBody } from "@utils";
import { MetadataValidationStatus } from "@models";
import { useWalletErrorModal } from "@hooks";
import { DRepDataFormValues } from "@/types/dRep";
import { useValidateMutation } from "../mutations";

export const defaultEditDRepInfoValues: DRepDataFormValues = {
  doNotList: false,
  givenName: "",
  objectives: "",
  motivations: "",
  qualifications: "",
  paymentAddress: "",
  linkReferences: [{ "@type": "Link", uri: "", label: "" }],
  identityReferences: [{ "@type": "Identity", uri: "", label: "" }],
  storeData: false,
  storingURL: "",
};

export const useEditDRepInfoForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  // Local state
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);

  // DApp Connector
  const { buildDRepUpdateCert, buildSignSubmitConwayCertTx } = useCardano();

  // App Management
  const { closeModal, openModal } = useModal();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const openWalletErrorModal = useWalletErrorModal();
  const { cExplorerBaseUrl } = useAppContext();

  // Queries
  const { validateMetadata } = useValidateMutation();

  // Form
  const {
    control,
    getValues,
    handleSubmit,
    formState: { errors, isValid },
    register,
    reset,
    watch,
  } = useFormContext<DRepDataFormValues>();
  const givenName = watch("givenName");
  const isError = Object.keys(errors).length > 0;

  // Navigation
  const backToForm = useCallback(() => {
    window.scrollTo(0, 0);
    setStep?.(1);
    closeModal();
  }, [setStep]);

  const backToDashboard = useCallback(() => {
    navigate(PATHS.dashboard);
    closeModal();
  }, []);

  // Business Logic
  const generateMetadata = useCallback(async () => {
    const { linkReferences, identityReferences, ...rest } = getValues();
    const body = generateMetadataBody({
      data: {
        ...rest,
        references: [...(linkReferences ?? []), ...(identityReferences ?? [])],
      },
      acceptedKeys: [
        "givenName",
        "objectives",
        "motivations",
        "qualifications",
        "paymentAddress",
        "doNotList",
      ],
    });

    const jsonld = await generateJsonld(body, DREP_CONTEXT);

    const jsonHash = blake2bHex(JSON.stringify(jsonld, null, 2), undefined, 32);

    setHash(jsonHash);
    setJson(jsonld);

    return jsonld;
  }, []);

  const onClickDownloadJson = async () => {
    if (!json) return;
    downloadJson(json, givenName);
  };

  const showLoadingModal = useCallback(() => {
    openModal({
      type: "loadingModal",
      state: {
        title: t("modals.pendingValidation.title"),
        message: t("modals.pendingValidation.message"),
        dataTestId: "storing-information-loading",
      },
    });
  }, []);

  const showSuccessModal = useCallback((link: string) => {
    openModal({
      type: "statusModal",
      state: {
        link: `${cExplorerBaseUrl}/tx/${link}`,
        status: "success",
        title: t("modals.registration.title"),
        message: t("modals.registration.message"),
        buttonText: t("modals.common.goToDashboard"),
        dataTestId: "governance-action-submitted-modal",
        onSubmit: backToDashboard,
      },
    });
  }, []);

  const onSubmit = useCallback(
    async (data: DRepDataFormValues) => {
      const url = data.storingURL;

      try {
        if (!hash) throw MetadataValidationStatus.INVALID_HASH;

        setIsLoading(true);
        showLoadingModal();

        const { status } = await validateMetadata({
          url,
          hash,
        });

        if (status) {
          throw status;
        }

        const updateDRepMetadataCert = await buildDRepUpdateCert(url, hash);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder: updateDRepMetadataCert,
          type: "updateMetaData",
        });

        if (result) showSuccessModal(result);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        if (Object.values(MetadataValidationStatus).includes(error)) {
          openModal({
            type: "statusModal",
            state: {
              ...storageInformationErrorModals[
                error as MetadataValidationStatus
              ],
              onSubmit: backToForm,
              onCancel: backToDashboard,
            },
          });
        } else {
          Sentry.setTag("hook", "useEditDRepInfoForm");
          Sentry.captureException(error);

          openWalletErrorModal({
            error: error?.message ? error.message : JSON.stringify(error),
            onSumbit: () => backToDashboard(),
            dataTestId: "edit-drep-transaction-error-modal",
          });
        }
      } finally {
        setIsLoading(false);
      }
    },
    [buildDRepUpdateCert, buildSignSubmitConwayCertTx, hash],
  );

  return {
    control,
    errors,
    generateMetadata,
    getValues,
    isError,
    isEditDRepMetadataLoading: isLoading,
    isValid,
    onClickDownloadJson,
    register,
    editDRepInfo: handleSubmit(onSubmit),
    watch,
    reset,
  };
};
