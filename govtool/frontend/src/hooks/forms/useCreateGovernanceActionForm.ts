import {
  Dispatch,
  SetStateAction,
  useCallback,
  useEffect,
  useState,
} from "react";
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
  getSurveyHash,
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
  attachSurvey?: boolean;
  surveyDetailsJson?: string;
} & Partial<Record<keyof GovernanceActionFieldSchemas, string>>;

export const defaulCreateGovernanceActionValues: CreateGovernanceActionValues =
  {
    references: [{ uri: "" }],
    storeData: false,
    storingURL: "",
    attachSurvey: false,
    surveyDetailsJson: "",
  };

const protocolParams = getItemFromLocalStorage(PROTOCOL_PARAMS_KEY);

type SurveyRef = {
  surveyTxId: string;
  surveyHash: string;
};

export const useCreateGovernanceActionForm = (
  setStep?: Dispatch<SetStateAction<number>>,
) => {
  // Local state
  const [isLoading, setIsLoading] = useState<boolean>(false);
  const [json, setJson] = useState<NodeObject | null>(null);
  const [createdSurveyRef, setCreatedSurveyRef] = useState<SurveyRef | null>(
    null,
  );

  // DApp Connector
  const {
    buildNewInfoGovernanceAction,
    buildTreasuryGovernanceAction,
    buildNoConfidenceGovernanceAction,
    buildNewConstitutionGovernanceAction,
    buildUpdateCommitteeGovernanceAction,
    buildSignSubmitConwayCertTx,
    buildMetadataAuxiliaryData,
    buildHardForkGovernanceAction,
    buildProtocolParameterChangeGovernanceAction,
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
  const attachSurvey = watch("attachSurvey");

  useEffect(() => {
    if (govActionType === GovernanceActionType.ParameterChange) {
      setValue("protocolParameters", JSON.stringify(protocolParams));
    }
  }, [govActionType]);

  useEffect(() => {
    if (!attachSurvey || govActionType !== GovernanceActionType.InfoAction) {
      setCreatedSurveyRef(null);
    }
  }, [attachSurvey, govActionType]);

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
  const generateMetadata = useCallback(
    async (surveyRef?: SurveyRef | null) => {
      if (!govActionType) {
        throw new Error("Governance action type is not defined");
      }

      const body = await generateMetadataBody({
        data: getValues(),
        acceptedKeys: ["title", "motivation", "abstract", "rationale"],
      });

      const jsonld = await generateJsonld(body, GOVERNANCE_ACTION_CONTEXT);
      const currentSurveyRef = surveyRef ?? createdSurveyRef;
      const shouldAttachSurveyRef =
        govActionType === GovernanceActionType.InfoAction && currentSurveyRef;

      const payload = shouldAttachSurveyRef
        ? {
            ...jsonld,
            specVersion: "1.0.0",
            kind: "cardano-governance-survey-link",
            surveyRef: {
              surveyTxId: currentSurveyRef.surveyTxId,
              surveyHash: currentSurveyRef.surveyHash,
            },
          }
        : jsonld;

      const jsonHash = blake2bHex(
        JSON.stringify(payload, null, 2),
        undefined,
        32,
      );

      setJson(payload);

      return {
        payload,
        hash: jsonHash,
      };
    },
    [getValues, govActionType, createdSurveyRef],
  );

  const parseSurveyDetails = useCallback((rawJson: string | undefined) => {
    if (!rawJson?.trim()) {
      throw new Error("Survey details JSON is required.");
    }

    let parsed: unknown;
    try {
      parsed = JSON.parse(rawJson);
    } catch (_error) {
      throw new Error("Survey details JSON must be valid JSON.");
    }

    if (!parsed || typeof parsed !== "object" || Array.isArray(parsed)) {
      throw new Error("Survey details JSON must be an object.");
    }

    const details = parsed as Record<string, unknown>;
    if (details.specVersion !== "1.0.0") {
      throw new Error("surveyDetails.specVersion must be 1.0.0.");
    }
    if (!details.title || typeof details.title !== "string") {
      throw new Error("surveyDetails.title is required.");
    }
    if (!details.description || typeof details.description !== "string") {
      throw new Error("surveyDetails.description is required.");
    }
    if (
      !Array.isArray(details.questions) ||
      (details.questions as unknown[]).length === 0
    ) {
      throw new Error("surveyDetails.questions must be a non-empty array.");
    }

    return details;
  }, []);

  const onClickDownloadJson = useCallback(() => {
    if (!json) return;
    downloadJson(json, govActionType);
  }, [govActionType, json]);

  const buildTransaction = useCallback(
    async (data: CreateGovernanceActionValues, metadataHash: string) => {
      const commonGovActionDetails = {
        hash: metadataHash,
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
    [
      buildHardForkGovernanceAction,
      buildNewConstitutionGovernanceAction,
      buildNewInfoGovernanceAction,
      buildNoConfidenceGovernanceAction,
      buildProtocolParameterChangeGovernanceAction,
      buildTreasuryGovernanceAction,
      buildUpdateCommitteeGovernanceAction,
      govActionType,
      t,
    ],
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
        const shouldAttachSurvey =
          data.attachSurvey && govActionType === GovernanceActionType.InfoAction;

        let surveyRefToUse = createdSurveyRef;
        if (shouldAttachSurvey) {
          const surveyDetails = parseSurveyDetails(data.surveyDetailsJson);
          const currentSurveyHash = getSurveyHash(surveyDetails).toLowerCase();
          const mustCreateSurveyTx =
            !surveyRefToUse || surveyRefToUse.surveyHash !== currentSurveyHash;

          if (mustCreateSurveyTx) {
            const auxiliaryData = buildMetadataAuxiliaryData(17, {
              surveyDetails,
            });
            const surveyTxId = await buildSignSubmitConwayCertTx({
              type: "createGovAction",
              auxiliaryData,
              skipStakeKeyRegistration: true,
              trackPending: false,
            });

            if (!surveyTxId) {
              throw new Error("Survey transaction was not submitted.");
            }

            surveyRefToUse = {
              surveyTxId,
              surveyHash: currentSurveyHash,
            };
            setCreatedSurveyRef(surveyRefToUse);
            setValue("storingURL", "");
            await generateMetadata(surveyRefToUse);

            openModal({
              type: "statusModal",
              state: {
                status: "info",
                title: "Survey transaction submitted",
                message:
                  "Your survey has been created. Download the updated metadata file, upload it, paste its URL, then submit again to create the Info Action.",
                buttonText: "Continue",
                dataTestId: "survey-created-info-modal",
              },
            });

            return;
          }
        }

        const metadata = await generateMetadata(surveyRefToUse);
        if (!metadata.hash) throw MetadataValidationStatus.INVALID_HASH;

        const { status } = await validateMetadata({
          url: data.storingURL,
          hash: metadata.hash,
        });

        if (status) {
          throw status;
        }

        const govActionBuilder = await buildTransaction(data, metadata.hash);
        const result = await buildSignSubmitConwayCertTx({
          govActionBuilder,
          type: "createGovAction",
        });

        if (result) {
          setCreatedSurveyRef(null);
          showSuccessModal(result);
        }
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
      buildMetadataAuxiliaryData,
      buildSignSubmitConwayCertTx,
      buildTransaction,
      createdSurveyRef,
      generateMetadata,
      govActionType,
      openModal,
      parseSurveyDetails,
      setValue,
      showLoadingModal,
      showSuccessModal,
      t,
      validateMetadata,
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
