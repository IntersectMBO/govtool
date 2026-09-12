import { useCallback, useMemo, useState } from "react";
import { useForm, useWatch } from "react-hook-form";
import * as Yup from "yup";
import { yupResolver } from "@hookform/resolvers/yup";
import { useLocation, useNavigate, useParams } from "react-router";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";
import { useWalletErrorModal } from "@hooks";
import { encodeMetadata, type SurveyResponse } from "cip-179";
import { ProposalVote, Vote } from "@/models";
import type { Cip179Participation } from "@/cip179/Cip179Survey";
import { metadatumCodec } from "@/cip179/csl";

export interface VoteActionFormValues {
  vote: string;
}

export const useVoteActionFormController = () => {
  const validationSchema = useMemo(
    () =>
      Yup.object().shape({
        vote: Yup.string().oneOf(["yes", "no", "abstain"]).required(),
      }),
    [],
  );

  return useForm<VoteActionFormValues>({
    defaultValues: { vote: "" },
    mode: "onChange",
    resolver: yupResolver<VoteActionFormValues>(validationSchema),
  });
};

type Props = {
  previousVote?: ProposalVote | null;
  voteContextHash?: string;
  voteContextUrl?: string;
  closeModal: () => void;
  cip179?: Cip179Participation;
};

export const useVoteActionForm = ({
  previousVote,
  closeModal,
  cip179,
}: Props) => {
  const [isLoading, setIsLoading] = useState(false);
  const { buildSignSubmitConwayCertTx, buildVote, isPendingTransaction } =
    useCardano();
  const { addSuccessAlert } = useSnackbar();
  const navigate = useNavigate();
  const { hash } = useLocation();
  const index = +hash.slice(1);
  const { proposalId: txHash } = useParams();
  const openWalletErrorModal = useWalletErrorModal();

  const {
    control,
    formState: { errors, isDirty },
    setValue,
    register: registerInput,
  } = useVoteActionFormController();

  const { vote } = useWatch({
    control,
  });

  const areFormErrors = !!errors.vote;

  const canVote =
    txHash !== undefined &&
    txHash !== null &&
    index !== undefined &&
    index !== null &&
    !areFormErrors;
  const canSubmitSurvey = !cip179?.participating || cip179.valid;

 const confirmVote = useCallback(
  async (
    voteValue?: Vote,
    url?: string,
    hashValue?: string | null,
  ) => {
    if (!canVote || !canSubmitSurvey || !voteValue) return;

    setIsLoading(true);

    const urlSubmitValue = url ?? "";
    const hashSubmitValue = hashValue ?? "";

    try {
      const isPendingTx = isPendingTransaction();
      if (isPendingTx) return;

      const votingBuilder = await buildVote(
        voteValue,
        txHash,
        index,
        urlSubmitValue,
        hashSubmitValue,
      );

      let surveyResponse: SurveyResponse | null = cip179?.response ?? null;
      if (
        surveyResponse &&
        cip179?.definition?.submissionMode.type === "sealed" &&
        surveyResponse.answers.type === "public"
      ) {
        const { isQuicknet, sealAnswers } = await import("cip-179/tlock");
        const mode = cip179.definition.submissionMode;
        if (!isQuicknet(mode.chainHash)) {
          throw new Error("This sealed survey uses an unsupported drand network");
        }
        const ciphertext = await sealAnswers(
          metadatumCodec,
          surveyResponse.answers.answers,
          mode.round,
          mode.paddingSize,
        );
        surveyResponse = {
          ...surveyResponse,
          answers: { type: "sealed", ciphertext },
        };
      }
      const encodedMetadata = surveyResponse
        ? encodeMetadata({ type: "responses", responses: [surveyResponse] })
        : undefined;
      const transactionMetadata =
        encodedMetadata instanceof Map ? encodedMetadata : undefined;

      const result = await buildSignSubmitConwayCertTx({
        votingBuilder,
        type: "vote",
        resourceId: txHash + index,
        transactionMetadata,
      });

      if (result) {
        addSuccessAlert("Vote submitted");
        navigate(PATHS.dashboardGovernanceActions, {
          state: {
            isVotedListOnLoad: !!previousVote?.vote,
          },
        });
        closeModal();
      }
    } catch (error) {
      openWalletErrorModal({
        error,
        dataTestId: "vote-transaction-error-modal",
      });
    } finally {
      setIsLoading(false);
    }
  },
    [
      buildVote,
      buildSignSubmitConwayCertTx,
      txHash,
      index,
      canVote,
      canSubmitSurvey,
      cip179,
    ],
  );

  return {
    confirmVote,
    setValue,
    vote,
    registerInput,
    isDirty,
    areFormErrors,
    isVoteLoading: isLoading,
    canVote: canVote && canSubmitSurvey,
  };
};
