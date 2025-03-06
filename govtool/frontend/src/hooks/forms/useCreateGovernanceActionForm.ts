import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { useNavigate } from "react-router-dom";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { useTranslation } from "react-i18next";
import { NodeObject } from "jsonld";

import {
  GOVERNANCE_ACTION_CONTEXT,
  PATHS,
  storageInformationErrorModals,
} from "@consts";
import { useCardano, useModal, useAppContext, QuorumThreshold } from "@context";
import {
  correctVoteAdaFormat,
  downloadJson,
  generateJsonld,
  generateMetadataBody,
  getItemFromLocalStorage,
  PROTOCOL_PARAMS_KEY,
} from "@utils";
import { useWalletErrorModal } from "@hooks";
import { MetadataValidationStatus } from "@models";
import {
  GovernanceActionFieldSchemas,
  GovernanceActionType,
} from "@/types/governanceAction";

import { useValidateMutation } from "../mutations";

export type CreateGovernanceActionValues = {
  references?: { uri: string }[];
  storeData?: boolean;
  storingURL: string;
  governance_action_type?: GovernanceActionType;
} & Partial<Record<keyof GovernanceActionFieldSchemas, string>>;

export const defaulCreateGovernanceActionValues: CreateGovernanceActionValues =
  {
    references: [{ uri: "" }],
    storeData: false,
    storingURL: "",
  };

const protocolParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

export const useCreateGovernanceActionForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  // Local state
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [hash, setHash] = useState<string | null>(null);
  const [json, setJson] = useState<NodeObject | null>(null);

  // DApp Connector
  const {
    buildNewInfoGovernanceAction,
    buildTreasuryGovernanceAction,
    buildNoConfidenceGovernanceAction,
    buildNewConstitutionGovernanceAction,
    buildUpdateCommitteeGovernanceAction,
    buildSignSubmitConwayCertTx,
  } = useCardano();

  // App Management
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { openModal, closeModal } = useModal();
  const openWalletErrorModal = useWalletErrorModal();
  const { cExplorerBaseUrl } = useAppContext();

  // Queries
  const { validateMetadata } = useValidateMutation();

  // Form
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

  // Navigation
  const backToForm = useCallback(() => {
    setStep?.(3);
    closeModal();
  }, [setStep]);

  const backToDashboard = useCallback(() => {
    navigate(PATHS.dashboard);
    closeModal();
  }, []);

  // Business Logic
  const generateMetadata = useCallback(async () => {
    if (!govActionType) {
      throw new Error("Governance action type is not defined");
    }

    const body = await generateMetadataBody({
      data: getValues(),
      acceptedKeys: ["title", "motivation", "abstract", "rationale"],
    });

    const jsonld = await generateJsonld(body, GOVERNANCE_ACTION_CONTEXT);

    const jsonHash = blake2bHex(JSON.stringify(jsonld, null, 2), undefined, 32);

    // That allows to validate metadata hash
    setHash(jsonHash);
    setJson(jsonld);

    return jsonld;
  }, [getValues]);

  const onClickDownloadJson = useCallback(() => {
    if (!json) return;
    downloadJson(json, govActionType);
  }, [govActionType, json]);

  const buildTransaction = useCallback(
    async (data: CreateGovernanceActionValues) => {
      if (!hash) return;

      const commonGovActionDetails = {
        hash,
        url: data.storingURL,
      };
      switch (govActionType) {
        case GovernanceActionType.InfoAction:
          return buildNewInfoGovernanceAction(commonGovActionDetails);
        case GovernanceActionType.NoConfidence:
          return buildNoConfidenceGovernanceAction(commonGovActionDetails);
        case GovernanceActionType.NewConstitution: {
          if (
            data.constitutionUrl === undefined ||
            data.constitutionHash === undefined
          ) {
            throw new Error(
              t("errors.invalidNewCommitteeGovernanceActionType"),
            );
          }

          return buildNewConstitutionGovernanceAction({
            ...commonGovActionDetails,
            constitutionUrl: data.constitutionUrl,
            constitutionHash: data.constitutionHash,
            scriptHash: data.scriptHash,
            prevGovernanceActionHash: data.prevGovernanceActionHash,
            prevGovernanceActionIndex: data.prevGovernanceActionIndex,
          });
        }
        case GovernanceActionType.NewCommittee: {
          if (
            data.newCommitteeHash === undefined ||
            data.newCommitteeExpiryEpoch === undefined
          ) {
            throw new Error(
              t("errors.invalidUpdateCommitteeGovernanceActionType"),
            );
          }

          let quorumThreshold: QuorumThreshold = {
            numerator: "1",
            denominator: "2",
          };
          if (data.numerator !== undefined && data.denominator !== undefined) {
            quorumThreshold = {
              numerator: data.numerator,
              denominator: data.denominator,
            };
          }

          return buildUpdateCommitteeGovernanceAction({
            ...commonGovActionDetails,
            newCommittee: [
              {
                committee: data.newCommitteeHash,
                expiryEpoch: data.newCommitteeExpiryEpoch,
              },
            ],
            removeCommittee: data.removeCommitteeHash
              ? [data.removeCommitteeHash]
              : [],
            quorumThreshold,
            prevGovernanceActionHash: data.prevGovernanceActionHash,
            prevGovernanceActionIndex: data.prevGovernanceActionIndex,
          });
        }
        case GovernanceActionType.TreasuryWithdrawals: {
          if (
            data.amount === undefined ||
            data.receivingAddress === undefined
          ) {
            throw new Error(t("errors.invalidTreasuryGovernanceActionType"));
          }

          const treasuryActionDetails = {
            ...commonGovActionDetails,
            withdrawals: [
              {
                amount: data.amount,
                receivingAddress: data.receivingAddress,
              },
            ],
          };

          return buildTreasuryGovernanceAction(treasuryActionDetails);
        }
        default:
          throw new Error(t("errors.invalidGovernanceActionType"));
      }
    },
    [hash],
  );

  const showSuccessModal = useCallback((link: string) => {
    openModal({
      type: "statusModal",
      state: {
        link: `${cExplorerBaseUrl}/tx/${link}`,
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

  const onSubmit = useCallback(
    async (data: CreateGovernanceActionValues) => {
      try {
        setIsLoading(true);
        showLoadingModal();
        if (!hash) throw MetadataValidationStatus.INVALID_HASH;
        const { status } = await validateMetadata({
          url: data.storingURL,
          hash,
        });

        if (status) {
          throw status;
        }

        const govActionBuilder = await buildTransaction(data);
        const result = await buildSignSubmitConwayCertTx({
          govActionBuilder,
          type: "createGovAction",
        });

        if (result) showSuccessModal(result);
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
      } catch (error: any) {
        const isInsufficientBalance = error
          ?.toLowerCase()
          ?.includes("insufficient");

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
            },
          });
        } else {
          openWalletErrorModal({
            error: isInsufficientBalance
              ? t("errors.insufficientBalanceDescription", {
                  ada: correctVoteAdaFormat(protocolParams?.gov_action_deposit),
                })
              : error,
            title: isInsufficientBalance
              ? t("errors.insufficientBalanceTitle")
              : undefined,
            dataTestId: "create-governance-action-error-modal",
          });
        }
      } finally {
        setIsLoading(false);
      }
    },
    [hash, buildTransaction, buildSignSubmitConwayCertTx],
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
