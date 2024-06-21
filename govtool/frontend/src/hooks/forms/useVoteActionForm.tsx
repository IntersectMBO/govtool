import { useCallback, useMemo, useState } from "react";
import { useForm, useWatch } from "react-hook-form";
import * as Yup from "yup";
import { yupResolver } from "@hookform/resolvers/yup";
import { useLocation, useNavigate, useParams } from "react-router-dom";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";
import { useWalletErrorModal } from "@hooks";

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

export const useVoteActionForm = (
  voteContextHash?: string,
  voteContextUrl?: string,
) => {
  const [isLoading, setIsLoading] = useState(false);
  const { buildSignSubmitConwayCertTx, buildVote, isPendingTransaction } =
    useCardano();
  const { addSuccessAlert } = useSnackbar();
  const navigate = useNavigate();
  const { state, hash } = useLocation();
  const params = useParams();
  const openWalletErrorModal = useWalletErrorModal();

  const {
    control,
    handleSubmit,
    formState: { errors, isDirty },
    setValue,
    register: registerInput,
  } = useVoteActionFormController();

  const watch = useWatch({
    control,
  });

  const areFormErrors = !!errors.vote;

  const confirmVote = useCallback(
    async (values: VoteActionFormValues) => {
      setIsLoading(true);

      const { vote } = values;

      const urlSubmitValue = voteContextUrl ?? "";
      const hashSubmitValue = voteContextHash ?? "";
      const txHash = state?.txHash ?? params.proposalId;
      const index = state?.index ?? hash.replace("#", "");

      try {
        const isPendingTx = isPendingTransaction();
        if (isPendingTx) return;
        const votingBuilder = await buildVote(
          vote,
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
              isVotedListOnLoad: !!(state && state.vote),
            },
          });
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
    [state, buildVote, buildSignSubmitConwayCertTx, params, hash],
  );

  return {
    confirmVote: handleSubmit(confirmVote),
    setValue,
    vote: watch.vote,
    registerInput,
    isDirty,
    areFormErrors,
    isVoteLoading: isLoading,
  };
};
