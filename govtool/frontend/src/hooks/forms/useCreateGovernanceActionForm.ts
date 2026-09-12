import {
  Dispatch,
  SetStateAction,
  useCallback,
  useEffect,
  useState,
} from "react";
import { useNavigate } from "react-router";
import { useFormContext } from "react-hook-form";
import { blake2bHex } from "blakejs";
import { useTranslation } from "react-i18next";
import { NodeObject } from "jsonld";

import {
  GOVERNANCE_ACTION_CONTEXT,
  GOVERNANCE_ACTION_CONTEXT_WITH_CIP179,
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
import { API } from "@services";
import {
  decodeDefinition,
  getRenderabilityProblem,
  type SurveyDefinitionEnvelope,
} from "@/cip179/core";
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
  surveyTxId?: string;
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
    buildHardForkGovernanceAction,
    buildProtocolParameterChangeGovernanceAction,
  } = useCardano();

  // App Management
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { openModal, closeModal } = useModal();
  const openWalletErrorModal = useWalletErrorModal();
  const { cExplorerBaseUrl, epochParams } = useAppContext();

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

  useEffect(() => {
    if (govActionType === GovernanceActionType.ParameterChange) {
      setValue("protocolParameters", JSON.stringify(protocolParams));
    }
  }, [govActionType]);

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
  const validateSurveyLink = useCallback(
    async (surveyTxId?: string) => {
      const normalized = surveyTxId?.trim().toLowerCase();
      if (!normalized) return;
      if (!/^[0-9a-f]{64}$/.test(normalized)) {
        throw new Error("Survey transaction hash must be 64 hexadecimal characters");
      }
      if (
        epochParams?.epoch_no === null ||
        epochParams?.epoch_no === undefined ||
        epochParams.gov_action_lifetime === null ||
        epochParams.gov_action_lifetime === undefined
      ) {
        throw new Error("Current governance action lifetime is unavailable");
      }
      const { data } = await API.get<SurveyDefinitionEnvelope>(
        `/survey/definition/${normalized}/0`,
      );
      const actionExpiration =
        epochParams.epoch_no + epochParams.gov_action_lifetime + 1;
      const definition = decodeDefinition(
        data,
        { txId: normalized, index: 0 },
        actionExpiration,
      );
      const renderabilityProblem = getRenderabilityProblem(definition);
      if (renderabilityProblem) throw new Error(renderabilityProblem);
    },
    [epochParams?.epoch_no, epochParams?.gov_action_lifetime],
  );

  const generateMetadata = useCallback(async () => {
    if (!govActionType) {
      throw new Error("Governance action type is not defined");
    }

    const values = getValues();
    await validateSurveyLink(values.surveyTxId);
    const body = await generateMetadataBody({
      data: values,
      acceptedKeys: ["title", "motivation", "abstract", "rationale"],
    });
    if (!body) throw new Error("Could not generate governance metadata");
    if (values.surveyTxId?.trim()) {
      body.cip179 = {
        specVersion: 5,
        kind: "survey-link",
        surveyTxId: values.surveyTxId.trim().toLowerCase(),
        surveyIndex: 0,
      };
    }

    const context = values.surveyTxId?.trim()
      ? GOVERNANCE_ACTION_CONTEXT_WITH_CIP179
      : GOVERNANCE_ACTION_CONTEXT;
    const jsonld = await generateJsonld(body, context);

    const jsonHash = blake2bHex(JSON.stringify(jsonld, null, 2), undefined, 32);

    // That allows to validate metadata hash
    setHash(jsonHash);
    setJson(jsonld);

    return jsonld;
  }, [getValues, validateSurveyLink]);

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
        case GovernanceActionType.HardForkInitiation: {
          if (
            data.major === undefined ||
            data.minor === undefined ||
            data.prevGovernanceActionHash === undefined ||
            data.prevGovernanceActionIndex === undefined
          ) {
            throw new Error(
              t("errors.invalidHardForkInitiationGovernanceActionType"),
            );
          }
          const hardForkActionDetails = {
            ...commonGovActionDetails,
            prevGovernanceActionHash: data.prevGovernanceActionHash,
            prevGovernanceActionIndex: data.prevGovernanceActionIndex,
            major: data.major,
            minor: data.minor,
          };
          return buildHardForkGovernanceAction(hardForkActionDetails);
        }

        case GovernanceActionType.ParameterChange: {
          if (
            data.protocolParameters === undefined ||
            data.prevGovernanceActionHash === undefined ||
            data.prevGovernanceActionIndex === undefined
          ) {
            throw new Error(
              t("errors.invalidParameterChangeGovernanceActionType"),
            );
          }
          const protocolParamsUpdate = JSON.parse(data.protocolParameters);
          const parameterChangeActionDetails = {
            ...commonGovActionDetails,
            protocolParamsUpdate,
            prevGovernanceActionHash: data.prevGovernanceActionHash,
            prevGovernanceActionIndex: data.prevGovernanceActionIndex,
          };
          return buildProtocolParameterChangeGovernanceAction(
            parameterChangeActionDetails,
          );
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
        await validateSurveyLink(data.surveyTxId);
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
    [
      hash,
      buildTransaction,
      buildSignSubmitConwayCertTx,
      validateSurveyLink,
    ],
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
