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
  PATHS,
  storageInformationErrorModals,
} from "@consts";
import { useCardano, useModal } from "@context";
import { MetadataValidationStatus } from "@models";
import { canonizeJSON, downloadJson, generateJsonld } from "@utils";

import { useGetVoterInfo } from "..";
import { useValidateMutation } from "../mutations";

export type RegisterAsDRepValues = {
  bio?: string;
  dRepName: string;
  email?: string;
  links?: Array<{ link: string }>;
  storeData?: boolean;
  storingURL: string;
};

export const defaultRegisterAsDRepValues: RegisterAsDRepValues = {
  bio: "",
  dRepName: "",
  email: "",
  links: [{ link: "" }],
  storeData: false,
  storingURL: "",
};

export const useRegisterAsdRepForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  const { validateMetadata } = useValidateMutation();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);
  const { closeModal, openModal } = useModal();
  const { buildDRepRegCert, buildDRepUpdateCert, buildSignSubmitConwayCertTx } =
    useCardano();
  const { voter } = useGetVoterInfo();

  const backToForm = useCallback(() => {
    setStep?.(2);
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
  } = useFormContext<RegisterAsDRepValues>();

  const dRepName = watch("dRepName");
  const isError = Object.keys(errors).length > 0;

  const generateMetadata = useCallback(async () => {
    const data = getValues();
    const acceptedKeys = ["dRepName", "bio", "email"];

    const filteredData = Object.entries(data)
      .filter(([key]) => acceptedKeys.includes(key))
      .map(([key, value]) => [CIP_QQQ + key, value]);

    const references = (data as RegisterAsDRepValues).links
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
    async (url: string) => {
      try {
        if (!hash) throw new Error(MetadataValidationStatus.INVALID_HASH);

        const result = await validateMetadata({ url, hash });

        if (result.status) {
          throw result.status;
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        if (
          Object.values(MetadataValidationStatus).includes(
            error as MetadataValidationStatus,
          )
        ) {
          openModal({
            type: "statusModal",
            state: {
              ...storageInformationErrorModals[
                error as MetadataValidationStatus
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

  const createRegistrationCert = useCallback(
    async (data: RegisterAsDRepValues) => {
      if (!hash) return;
      const url = data.storingURL;

      try {
        if (voter?.isRegisteredAsSoleVoter) {
          return await buildDRepUpdateCert(url, hash);
        }

        return await buildDRepRegCert(url, hash);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        console.error(error);
        throw error;
      }
    },
    [
      buildDRepRegCert,
      buildDRepUpdateCert,
      hash,
      voter?.isRegisteredAsSoleVoter,
    ],
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
    async (data: RegisterAsDRepValues) => {
      try {
        setIsLoading(true);

        await validateHash(data.storingURL);
        const registerAsDRepCert = await createRegistrationCert(data);
        await buildSignSubmitConwayCertTx({
          certBuilder: registerAsDRepCert,
          type: "registerAsDrep",
        });

        showSuccessModal();
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        captureException(error);
      } finally {
        setIsLoading(false);
      }
    },
    [buildSignSubmitConwayCertTx, createRegistrationCert, hash, validateHash],
  );

  return {
    control,
    errors,
    generateMetadata,
    getValues,
    isError,
    isRegistrationAsDRepLoading: isLoading,
    isValid,
    onClickDownloadJson,
    register,
    registerAsDrep: handleSubmit(onSubmit),
    resetField,
    watch,
  };
};
