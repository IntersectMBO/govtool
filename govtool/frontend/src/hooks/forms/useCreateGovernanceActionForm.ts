import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { captureException } from "@sentry/react";
import { useTranslation } from "react-i18next";
import { NodeObject } from "jsonld";

import {
  CIP_100,
  CIP_108,
  GOVERNANCE_ACTION_CONTEXT,
  MetadataHashValidationErrors,
  PATHS,
  storageInformationErrorModals,
} from "@consts";
import {
  GovernanceActionFieldSchemas,
  GovernanceActionType,
} from "@/types/governanceAction";
import {
  canonizeJSON,
  downloadJson,
  generateJsonld,
  validateMetadataHash,
} from "@/utils";
import { useCardano, useModal } from "@/context";

export type CreateGovernanceActionValues = {
  links?: { link: string }[];
  storeData?: boolean;
  storingURL: string;
  governance_action_type?: GovernanceActionType;
} & Partial<Record<keyof GovernanceActionFieldSchemas, string>>;

export const defaulCreateGovernanceActionValues: CreateGovernanceActionValues =
  {
    links: [{ link: "" }],
    storeData: false,
    storingURL: "",
  };

export const useCreateGovernanceActionForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  const {
    buildNewInfoGovernanceAction,
    buildTreasuryGovernanceAction,
    buildSignSubmitConwayCertTx,
  } = useCardano();
  const { t } = useTranslation();
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);
  const navigate = useNavigate();
  const { openModal, closeModal } = useModal();
  const {
    control,
    formState: { errors, isValid },
    getValues,
    handleSubmit,
    setValue,
    watch,
    register,
    reset,
  } = useFormContext<CreateGovernanceActionValues>();
  const govActionType = watch("governance_action_type");

  const backToForm = useCallback(() => {
    setStep?.(3);
    closeModal();
  }, [setStep]);

  const backToDashboard = useCallback(() => {
    navigate(PATHS.dashboard);
    closeModal();
  }, []);

  const generateMetadata = useCallback(async () => {
    const data = getValues();

    if (!govActionType) {
      throw new Error("Governance action type is not defined");
    }

    const acceptedKeys = ["title", "motivation", "abstract", "rationale"];

    const filteredData = Object.entries(data)
      .filter(([key]) => acceptedKeys.includes(key))
      .map(([key, value]) => [CIP_108 + key, value]);

    const references = (data as CreateGovernanceActionValues).links
      ?.filter((link) => link.link)
      .map((link) => ({
        "@type": "Other",
        [`${CIP_100}reference-label`]: "Label",
        [`${CIP_100}reference-uri`]: link.link,
      }));

    const body = {
      ...Object.fromEntries(filteredData),
      [`${CIP_108}references`]: references,
    };

    const jsonld = await generateJsonld(body, GOVERNANCE_ACTION_CONTEXT);

    const canonizedJson = await canonizeJSON(jsonld);
    const canonizedJsonHash = blake2bHex(canonizedJson, undefined, 32);

    // That allows to validate metadata hash
    setHash(canonizedJsonHash);
    setJson(jsonld);

    return jsonld;
  }, [getValues]);

  const onClickDownloadJson = useCallback(() => {
    if (!json) return;
    downloadJson(json, govActionType);
  }, [govActionType, json]);

  const validateHash = useCallback(
    async (storingUrl: string, localHash: string | null) => {
      try {
        if (!localHash) {
          throw new Error(MetadataHashValidationErrors.INVALID_HASH);
        }
        await validateMetadataHash(storingUrl, localHash);
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
    [backToForm],
  );

  const buildTransaction = useCallback(
    async (data: CreateGovernanceActionValues) => {
      if (!hash) return;

      const commonGovActionDetails = {
        hash,
        url: data.storingURL,
      };
      try {
        switch (govActionType) {
          case GovernanceActionType.Info:
            return await buildNewInfoGovernanceAction(commonGovActionDetails);
          case GovernanceActionType.Treasury: {
            if (
              data.amount === undefined ||
              data.receivingAddress === undefined
            ) {
              throw new Error(t("errors.invalidTreasuryGovernanceActionType"));
            }

            const treasuryActionDetails = {
              ...commonGovActionDetails,
              amount: data.amount,
              receivingAddress: data.receivingAddress,
            };

            return await buildTreasuryGovernanceAction(treasuryActionDetails);
          }
          default:
            throw new Error(t("errors.invalidGovernanceActionType"));
        }
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        // eslint-disable-next-line no-console
        console.error(error);
        throw error;
      }
    },
    [hash],
  );

  const showSuccessModal = useCallback(() => {
    openModal({
      type: "statusModal",
      state: {
        status: "success",
        title: t(
          "createGovernanceAction.modals.submitTransactionSuccess.title",
        ),
        message: t(
          "createGovernanceAction.modals.submitTransactionSuccess.message",
        ),
        buttonText: t("modals.common.goToDashboard"),
        dataTestId: "governance-action-submitted-modal",
        onSubmit: backToDashboard,
      },
    });
  }, []);

  const onSubmit = useCallback(
    async (data: CreateGovernanceActionValues) => {
      try {
        setIsLoading(true);

        await validateHash(data.storingURL, hash);
        const govActionBuilder = await buildTransaction(data);
        await buildSignSubmitConwayCertTx({ govActionBuilder });

        showSuccessModal();
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        captureException(error);
      } finally {
        setIsLoading(false);
      }
    },
    [hash],
  );

  return {
    control,
    createGovernanceAction: handleSubmit(onSubmit),
    errors,
    generateMetadata,
    getValues,
    isLoading,
    isValid,
    onClickDownloadJson,
    register,
    reset,
    setValue,
    watch,
  };
};
