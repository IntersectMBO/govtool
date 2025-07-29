import { Dispatch, SetStateAction, useMemo } from "react";

import { orange } from "@consts";
import { Typography } from "@atoms";
import { VoteContextWrapper } from "@organisms";
import { useTranslation, useVoteContextForm } from "@/hooks";
import { ControlledField } from "..";
import { Vote } from "@/models";

type VoteContextTextProps = {
  setStep: Dispatch<SetStateAction<number>>;
  onCancel: () => void;
  confirmVote: (
    vote?: Vote,
    url?: string,
    hash?: string | null,
  ) => void;
  vote?: Vote;
  previousRationale? : string
};
const MAX_LENGTH = 10000;

export const VoteContextText = ({
  setStep,
  onCancel,
  confirmVote,
  vote,
  previousRationale
}: VoteContextTextProps) => {
  const { t } = useTranslation();

  const { control, errors, watch } = useVoteContextForm();
  const currentRationale = watch("voteContextText");

  const isRationaleChanged = useMemo(() => {
    console.log({"currentRationale":currentRationale,previousRationale:previousRationale})
    return currentRationale !== previousRationale;
  }, [currentRationale, previousRationale]);

  const buttonLabel = useMemo(() => {
    if (currentRationale === "") {
      return t("govActions.voting.voteWithoutMetadata");
    }
    return t("govActions.voting.continue");
  }, [currentRationale, t]);

  const fieldProps = {
    layoutStyles: { mb: 3 },
    name: "voteContextText",
    placeholder: t("govActions.provideContext"),
    rules: {
      maxLength: {
        value: MAX_LENGTH,
        message: t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: MAX_LENGTH,
        }),
      },
    },
  };
  console.log("Previous rationale",previousRationale)
  return (
    <VoteContextWrapper
      onContinue={() => setStep(2)}
      isVoteWithMetadata={currentRationale !== ""}
      onCancel={onCancel}
      onSkip={() => confirmVote(vote)}
      continueLabel={buttonLabel}
      isChangeVote={previousRationale !== undefined && previousRationale !== null}
      isRationaleChanged={isRationaleChanged}
    >
      <Typography
        variant="body1"
        sx={{
          textTransform: "uppercase",
          color: orange.c400,
        }}
      >
        {t("optional")}
      </Typography>
      <Typography
        variant="title2"
        sx={{
          lineHeight: "34px",
          mb: 1,
        }}
      >
        {t("govActions.provideContextAboutYourVote")}
      </Typography>
      <Typography variant="body1" sx={{ fontWeight: 400, mb: 2 }}>
        {t("govActions.additionalInformationAboutYourVote")}
      </Typography>
      <ControlledField.TextArea
        {...{ control, errors }}
        {...fieldProps}
        key="voteContextText"
        isModifiedLayout
        maxLength={MAX_LENGTH}
        data-testid="provide-context-input"
        defaultValue={previousRationale}
      />
    </VoteContextWrapper>
  );
};
