import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useTranslation } from "react-i18next";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { captureException } from "@sentry/react";
import { NodeObject } from "jsonld";

import {
  CIP_QQQ,
  DREP_CONTEXT,
  PATHS,
  storageInformationErrorModals,
} from "@consts";
import { useCardano, useModal } from "@context";
import { MetadataStandard, MetadataValidationStatus } from "@models";
import {
  canonizeJSON,
  downloadJson,
  ellipsizeText,
  generateJsonld,
  generateMetadataBody,
} from "@utils";

import { useGetVoterInfo, useWalletErrorModal } from "@hooks";
import { useValidateMutation } from "../mutations";

export type RegisterAsDRepValues = {
  bio?: string;
  dRepName: string;
  email?: string;
  references?: Array<{ link: string }>;
  storeData?: boolean;
  storingURL: string;
};

export const defaultRegisterAsDRepValues: RegisterAsDRepValues = {
  bio: "",
  dRepName: "",
  email: "",
  references: [{ link: "" }],
  storeData: false,
  storingURL: "",
};

export const useRegisterAsdRepForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  // Local state
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);

  // DApp Connector
  const {
    buildDRepRegCert,
    buildDRepUpdateCert,
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    dRepID,
  } = useCardano();

  // App Management
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { closeModal, openModal } = useModal();
  const openWalletErrorModal = useWalletErrorModal();

  // Queries
  const { validateMetadata } = useValidateMutation();
  const { voter } = useGetVoterInfo();

  // Form
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

  // Navigation
  const backToForm = useCallback(() => {
    window.scrollTo(0, 0);
    setStep?.(2);
    closeModal();
  }, [setStep]);

  const backToDashboard = useCallback(() => {
    navigate(PATHS.dashboard);
    closeModal();
  }, []);

  // Business Logic
  const generateMetadata = useCallback(async () => {
    const body = generateMetadataBody({
      data: getValues(),
      acceptedKeys: ["dRepName", "bio", "email"],
      standardReference: CIP_QQQ,
    });

    const jsonld = await generateJsonld(body, DREP_CONTEXT, CIP_QQQ);

    const canonizedJson = await canonizeJSON(jsonld);
    const canonizedJsonHash = blake2bHex(canonizedJson, undefined, 32);

    setHash(canonizedJsonHash);
    setJson(jsonld);

    return jsonld;
  }, []);

  const onClickDownloadJson = async () => {
    if (!json) return;

    downloadJson(json, ellipsizeText(dRepName, 16, ""));
  };

  const createRegistrationCert = useCallback(
    async (data: RegisterAsDRepValues) => {
      if (!hash) return;
      const url = data.storingURL;
      try {
        const certBuilder = await buildVoteDelegationCert(dRepID);

        const registerCert = voter?.isRegisteredAsSoleVoter
          ? await buildDRepUpdateCert(url, hash)
          : await buildDRepRegCert(url, hash);

        certBuilder.add(registerCert);

        return certBuilder;
      } catch (error) {
        captureException(error);
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
        link: `https://sancho.cexplorer.io/tx/${link}`,
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
        if (!hash) throw MetadataValidationStatus.INVALID_HASH;

        setIsLoading(true);
        showLoadingModal();

        const { status } = await validateMetadata({
          url: data.storingURL,
          hash,
          standard: MetadataStandard.CIPQQQ,
        });

        if (status) {
          throw status;
        }
        const registerAsDRepCert = await createRegistrationCert(data);
        const result = await buildSignSubmitConwayCertTx({
          certBuilder: registerAsDRepCert,
          type: "registerAsDrep",
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
          captureException(error);

          openWalletErrorModal({
            error,
            onSumbit: () => backToDashboard(),
            dataTestId: "registration-transaction-error-modal",
          });
        }
      } finally {
        setIsLoading(false);
      }
    },
    [buildSignSubmitConwayCertTx, createRegistrationCert, hash],
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
