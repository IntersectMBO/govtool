import { useCallback, useMemo, useState } from "react";
import { useForm, useWatch } from "react-hook-form";
import * as Yup from "yup";
import { yupResolver } from "@hookform/resolvers/yup";
import { useLocation, useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";

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
  const { addErrorAlert, addSuccessAlert } = useSnackbar();
  const navigate = useNavigate();
  const { state } = useLocation();

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

      try {
        const isPendingTx = isPendingTransaction();
        if (isPendingTx) return;
        const votingBuilder = await buildVote(
          vote,
          state.txHash,
          state.index,
          urlSubmitValue,
          hashSubmitValue,
        );
        const result = await buildSignSubmitConwayCertTx({
          votingBuilder,
          type: "vote",
          resourceId: state.txHash + state.index,
        });
        if (result) {
          addSuccessAlert("Vote submitted");
          navigate(PATHS.dashboardGovernanceActions, {
            state: {
              isVotedListOnLoad: !!(state && state.vote),
            },
          });
        }
      } catch (e) {
        addErrorAlert("Please try again later");
      } finally {
        setIsLoading(false);
      }
    },
    [state, buildVote, buildSignSubmitConwayCertTx],
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
