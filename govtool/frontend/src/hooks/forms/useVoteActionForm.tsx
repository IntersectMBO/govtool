import { useCallback, useMemo, useState } from "react";
import { useForm, useWatch } from "react-hook-form";
import * as Yup from "yup";
import { yupResolver } from "@hookform/resolvers/yup";
import { useLocation, useNavigate } from "react-router-dom";

import { PATHS } from "@consts";
import { useCardano, useSnackbar } from "@context";
import { HASH_REGEX, URL_REGEX } from "@utils";
import { useTranslation } from "@hooks";
import { UrlAndHashFormValues } from "./useUrlAndHashFormController";

export interface VoteActionFormValues extends UrlAndHashFormValues {
  vote: string;
}

export const useVoteActionFormController = () => {
  const { t } = useTranslation();

  const validationSchema = useMemo(
    () =>
      Yup.object().shape({
        vote: Yup.string().oneOf(["yes", "no", "abstain"]).required(),
        url: Yup.string()
          .trim()
          .max(64, t("forms.errors.urlTooLong"))
          .test(
            "url-validation",
            t("forms.errors.urlInvalidFormat"),
            (value) => !value || URL_REGEX.test(value),
          ),
        hash: Yup.string()
          .trim()
          .test(
            "hash-length-validation",
            t("forms.errors.hashInvalidLength"),
            (value) => !value || value.length === 64,
          )
          .test(
            "hash-format-validation",
            t("forms.errors.hashInvalidFormat"),
            (value) => !value || HASH_REGEX.test(value),
          ),
        storeData: Yup.boolean(),
      }),
    [],
  );

  return useForm<VoteActionFormValues>({
    defaultValues: { url: "", hash: "", vote: "" },
    mode: "onChange",
    resolver: yupResolver<VoteActionFormValues>(validationSchema),
  });
};

export const useVoteActionForm = () => {
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
    clearErrors,
  } = useVoteActionFormController();

  const watch = useWatch({
    control,
  });

  const areFormErrors = !!errors.vote || !!errors.url || !!errors.hash;

  const confirmVote = useCallback(
    async (values: VoteActionFormValues) => {
      setIsLoading(true);

      const { url, hash, vote } = values;

      const urlSubmitValue = url ?? "";
      const hashSubmitValue = hash ?? "";

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
    control,
    errors,
    confirmVote: handleSubmit(confirmVote),
    setValue,
    vote: watch.vote,
    registerInput,
    isDirty,
    clearErrors,
    areFormErrors,
    isVoteLoading: isLoading,
  };
};
