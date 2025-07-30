import { useCallback, useMemo, useState } from "react";
import { useForm, useWatch } from "react-hook-form";
import * as Yup from "yup";
import { yupResolver } from "@hookform/resolvers/yup";
import { useLocation, useNavigate, useParams } from "react-router-dom";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";
import { useWalletErrorModal } from "@hooks";
import { ProposalVote, Vote } from "@/models";

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
};

export const useVoteActionForm = ({
  previousVote,
  closeModal,
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

 const confirmVote = useCallback(
  async (
    voteValue?: Vote,
    url?: string,
    hashValue?: string | null,
  ) => {
    if (!canVote || !voteValue) return;

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

      const result = await buildSignSubmitConwayCertTx({
        votingBuilder,
        type: "vote",
        resourceId: txHash + index,
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
    [buildVote, buildSignSubmitConwayCertTx, txHash, index, canVote],
  );

  return {
    confirmVote,
    setValue,
    vote,
    registerInput,
    isDirty,
    areFormErrors,
    isVoteLoading: isLoading,
    canVote,
  };
};
