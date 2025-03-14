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
const MAX_LENGTH = 10000;

export const VoteContextText = ({
  setStep,
  onCancel,
}: VoteContextTextProps) => {
  const { t } = useTranslation();

  const { control, errors, watch } = useVoteContextForm();
  const isContinueDisabled = !watch("voteContextText");

  const fieldProps = {
    layoutStyles: { mb: 3 },
    name: "voteContextText",
    placeholder: t("govActions.provideContext"),
    rules: {
      required: {
        value: true,
        message: t("createGovernanceAction.fields.validations.required"),
      },
      maxLength: {
        value: MAX_LENGTH,
        message: t("createGovernanceAction.fields.validations.maxLength", {
          maxLength: MAX_LENGTH,
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
        {t("govActions.additionalInformationAboutYourVote")}
      </Typography>
      <ControlledField.TextArea
        {...{ control, errors }}
        {...fieldProps}
        key="voteContextText"
        isModifiedLayout
        maxLength={MAX_LENGTH}
        data-testid="provide-context-input"
      />
    </VoteContextWrapper>
  );
};
