import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { captureException } from "@sentry/react";
import { NodeObject } from "jsonld";

import {
  CIP_100,
  CIP_QQQ,
  DREP_CONTEXT,
  MetadataHashValidationErrors,
  PATHS,
  storageInformationErrorModals,
} from "@consts";
import { useCardano, useModal } from "@context";
import {
  canonizeJSON,
  downloadJson,
  generateJsonld,
  validateMetadataHash,
} from "@utils";

export type EditDRepInfoValues = {
  bio?: string;
  dRepName: string;
  email?: string;
  links?: Array<{ link: string }>;
  storeData?: boolean;
  storingURL: string;
};

export const defaultEditDRepInfoValues: EditDRepInfoValues = {
  bio: "",
  dRepName: "",
  email: "",
  links: [{ link: "" }],
  storeData: false,
  storingURL: "",
};

export const useEditDRepInfoForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  const { t } = useTranslation();
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);
  const { closeModal, openModal } = useModal();
  const { buildDRepUpdateCert, buildSignSubmitConwayCertTx } = useCardano();

  const backToForm = useCallback(() => {
    setStep?.(1);
    closeModal();
  }, [setStep]);

  const backToDashboard = useCallback(() => {
    navigate(PATHS.dashboard);
    closeModal();
  }, []);

  const {
    control,
    getValues,
    handleSubmit,
    formState: { errors, isValid },
    register,
    resetField,
    watch,
  } = useFormContext<EditDRepInfoValues>();

  const dRepName = watch("dRepName");
  const isError = Object.keys(errors).length > 0;

  const generateMetadata = useCallback(async () => {
    const data = getValues();
    const acceptedKeys = ["dRepName", "bio", "email"];

    const filteredData = Object.entries(data)
      .filter(([key]) => acceptedKeys.includes(key))
      .map(([key, value]) => [CIP_QQQ + key, value]);

    const references = (data as EditDRepInfoValues).links
      ?.filter((link) => link.link)
      .map((link) => ({
        "@type": "Other",
        [`${CIP_100}reference-label`]: "Label",
        [`${CIP_100}reference-uri`]: link.link,
      }));

    const body = {
      ...Object.fromEntries(filteredData),
      [`${CIP_QQQ}references`]: references,
    };

    const jsonld = await generateJsonld(body, DREP_CONTEXT, CIP_QQQ);

    const canonizedJson = await canonizeJSON(jsonld);
    const canonizedJsonHash = blake2bHex(canonizedJson, undefined, 32);

    setHash(canonizedJsonHash);
    setJson(jsonld);

    return jsonld;
  }, []);

  const onClickDownloadJson = async () => {
    if (!json) return;

    downloadJson(json, dRepName);
  };

  const validateHash = useCallback(
    async (storingUrl: string) => {
      try {
        if (!hash) throw new Error(MetadataHashValidationErrors.INVALID_HASH);

        await validateMetadataHash(storingUrl, hash);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        if (
          Object.values(MetadataHashValidationErrors).includes(error.message)
        ) {
          openModal({
            type: "statusModal",
            state: {
              ...storageInformationErrorModals[
                error.message as MetadataHashValidationErrors
              ],
              onSubmit: backToForm,
              onCancel: backToDashboard,
              // TODO: Open usersnap feedback
              onFeedback: backToDashboard,
            },
          });
        }
        throw error;
      }
    },
    [backToForm, hash],
  );

  const showSuccessModal = useCallback(() => {
    openModal({
      type: "statusModal",
      state: {
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
    async (data: EditDRepInfoValues) => {
      const url = data.storingURL;

      if (!hash) return;

      try {
        setIsLoading(true);

        await validateHash(url);
        const updateDRepMetadataCert = await buildDRepUpdateCert(url, hash);
        await buildSignSubmitConwayCertTx({
          certBuilder: updateDRepMetadataCert,
          type: "updateMetaData",
        });

        showSuccessModal();
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        captureException(error);
      } finally {
        setIsLoading(false);
      }
    },
    [buildDRepUpdateCert, buildSignSubmitConwayCertTx, hash, validateHash],
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
    resetField,
    watch,
  };
};
