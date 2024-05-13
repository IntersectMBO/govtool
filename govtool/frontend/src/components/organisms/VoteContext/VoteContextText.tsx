import { Dispatch, SetStateAction } from "react";

import { orange } from "@consts";
import { Typography } from "@atoms";
import { VoteContextWrapper } from "@organisms";
import { useTranslation, useVoteContextForm } from "@/hooks";
import { ControlledField } from "..";

type VoteContextTextProps = {
  setStep: Dispatch<SetStateAction<number>>;
  onCancel: () => void;
};

export const VoteContextText = ({
  setStep,
  onCancel,
}: VoteContextTextProps) => {
  const { t } = useTranslation();

  const { control, errors, watch } = useVoteContextForm();
  const isContinueDisabled = !watch("voteContextText");

  const fieldProps = {
    key: "voteContextText",
    layoutStyles: { mb: 3 },
    name: "voteContextText",
    placeholder: t("govActions.provideContext"),
    rules: {
      required: {
        value: true,
        message: t("createGovernanceAction.fields.validations.required"),
      },
      maxLength: {
        value: 500,
        message: t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: 500,
        }),
      },
    },
  };

  return (
    <VoteContextWrapper
      onContinue={() => setStep(2)}
      isContinueDisabled={isContinueDisabled}
      onCancel={onCancel}
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
        {/* TODO: Update text when design is finalised */}
        Additional information about your vote
      </Typography>
      <ControlledField.TextArea
        {...{ control, errors }}
        {...fieldProps}
        isModifiedLayout
        data-testid="provide-context-input"
      />
    </VoteContextWrapper>
  );
};
