import { Dispatch, SetStateAction, useCallback } from "react";
import { useFieldArray } from "react-hook-form";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { GOVERNANCE_ACTION_FIELDS, Placeholders } from "@consts";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons, Field } from "@molecules";
import { URL_REGEX, testIdFromLabel } from "@/utils";
import {
  GovernanceActionField,
  GovernanceActionType,
} from "@/types/governanceAction";

import { ControlledField } from "../ControlledField";

const MAX_NUMBER_OF_LINKS = 8;

type CreateGovernanceActionFormProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const CreateGovernanceActionForm = ({
  setStep,
}: CreateGovernanceActionFormProps) => {
  const { t } = useTranslation();
  const { control, errors, getValues, register, reset, watch } =
    useCreateGovernanceActionForm();

  const isError = Object.keys(errors).length > 0;

  const type = getValues("governance_action_type");
  const attachSurvey = watch("attachSurvey");
  const surveyDetailsText = watch("surveyDetailsJson") ?? "";

  const validateSurveyDetailsJson = useCallback(
    (value?: string) => {
      if (!attachSurvey || type !== GovernanceActionType.InfoAction) {
        return true;
      }

      if (!value?.trim()) {
        return t("createGovernanceAction.fields.validations.required");
      }

      try {
        const parsed = JSON.parse(value) as Record<string, unknown>;
        const questions = parsed.questions;
        if (
          parsed.specVersion !== "1.0.0" ||
          typeof parsed.title !== "string" ||
          typeof parsed.description !== "string" ||
          !Array.isArray(questions) ||
          !questions.length
        ) {
          return "Survey JSON must include specVersion, title, description, and questions.";
        }
      } catch (_error) {
        return "Survey JSON is invalid.";
      }

      return true;
    },
    [attachSurvey, t, type],
  );

  const isSurveySectionEnabled =
    type === GovernanceActionType.InfoAction && !!attachSurvey;
  const {
    append,
    fields: references,
    remove,
  } = useFieldArray({
    control,
    name: "references",
  });

  const isContinueButtonDisabled =
    // TODO: Provide better typing for GOVERNANCE_ACTION_FIELDS
    // to get rid of explicit type assertion
    Object.keys(
      GOVERNANCE_ACTION_FIELDS[
        type! as
          | GovernanceActionType.InfoAction
          | GovernanceActionType.TreasuryWithdrawals
          | GovernanceActionType.NewCommittee
          | GovernanceActionType.NewConstitution
          | GovernanceActionType.NoConfidence
          | GovernanceActionType.HardForkInitiation
          | GovernanceActionType.ParameterChange
      ],
    ).some(
      (field) => !watch(field as unknown as Parameters<typeof watch>[0]),
    ) ||
    (isSurveySectionEnabled && !surveyDetailsText.trim()) ||
    isError;

  const onClickContinue = () => {
    setStep(4);
  };

  const onClickBack = () => {
    reset();
    setStep(2);
  };

  const renderGovernanceActionField = () =>
    Object.entries(
      GOVERNANCE_ACTION_FIELDS[
        type! as
          | GovernanceActionType.InfoAction
          | GovernanceActionType.TreasuryWithdrawals
          | GovernanceActionType.NewCommittee
          | GovernanceActionType.NewConstitution
          | GovernanceActionType.NoConfidence
          | GovernanceActionType.HardForkInitiation
          | GovernanceActionType.ParameterChange
      ],
    ).map(([key, field]) => {
      const fieldProps = {
        helpfulText: field.tipI18nKey ? t(field.tipI18nKey) : undefined,
        key,
        label: t(field.labelI18nKey),
        layoutStyles: { mb: 3 },
        name: key,
        placeholder: field.placeholderI18nKey
          ? t(field.placeholderI18nKey)
          : undefined,
        rules: field.rules,
        maxLength: field.maxLength,
      };

      if (field.component === GovernanceActionField.Input) {
        return (
          <ControlledField.Input {...{ control, errors }} {...fieldProps} />
        );
      }
      if (field.component === GovernanceActionField.TextArea) {
        return (
          <ControlledField.TextArea
            {...{ control, errors }}
            {...fieldProps}
            data-testid={`${testIdFromLabel(fieldProps.label)}-input`}
          />
        );
      }
    });

  const addLink = useCallback(() => append({ uri: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const renderLinks = useCallback(
    () =>
      references.map((field, index) => (
        <ControlledField.Input
          {...register(`references.${index}.uri`)}
          errors={errors}
          endAdornment={
            references.length > 1 ? (
              <DeleteOutlineIcon
                color="primary"
                data-testid={`delete-link-${index + 1}-button`}
                onClick={() => removeLink(index)}
                sx={{ cursor: "pointer", height: 24, with: 24 }}
              />
            ) : null
          }
          key={field.id}
          label={`${t("forms.link")} ${index + 1}`}
          layoutStyles={{ mb: 3 }}
          placeholder={Placeholders.LINK}
          name={`references.${index}.uri`}
          rules={{
            pattern: {
              value: URL_REGEX,
              message: t("createGovernanceAction.fields.validations.url"),
            },
          }}
        />
      )),
    [references],
  );

  return (
    <>
      <InfoText label={t("required")} sx={{ mb: 0.75, textAlign: "center" }} />
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.formTitle")}
      </Typography>
      <Spacer y={4.25} />
      <Field.Input
        disabled
        helpfulText={t("forms.createGovernanceAction.typeTip")}
        label={t("forms.createGovernanceAction.typeLabel")}
        value={type}
      />
      <Spacer y={3} />
      {renderGovernanceActionField()}
      {type === GovernanceActionType.InfoAction && (
        <>
          <Spacer y={1} />
          <InfoText
            label={t("optional")}
            sx={{ mb: 0.75, textAlign: "center" }}
          />
          <Typography sx={{ textAlign: "center" }} variant="headline4">
            Survey Link (Info Action)
          </Typography>
          <Spacer y={2.5} />
          <ControlledField.Checkbox
            {...{ control, errors }}
            data-testid="attach-survey-checkbox"
            label="Create and attach an on-chain survey"
            name="attachSurvey"
          />
          {attachSurvey && (
            <>
              <Spacer y={2} />
              <ControlledField.TextArea
                {...{ control, errors }}
                data-testid="survey-details-json-input"
                helpfulText="Paste label 17 surveyDetails JSON only (no wrapper envelope)."
                label="Survey details JSON"
                layoutStyles={{ mb: 3 }}
                maxLength={20000}
                name="surveyDetailsJson"
                placeholder='{"specVersion":"1.0.0","title":"...","description":"...","questions":[...]}'
                rules={{
                  validate: validateSurveyDetailsJson,
                }}
              />
            </>
          )}
        </>
      )}
      <InfoText label={t("optional")} sx={{ mb: 0.75, textAlign: "center" }} />
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.references")}
      </Typography>
      <Spacer y={4.25} />
      {renderLinks()}
      {references?.length < MAX_NUMBER_OF_LINKS ? (
        <Button
          data-testid="add-link-button"
          onClick={addLink}
          size="extraLarge"
          variant="text"
        >
          {t("addLink")}
        </Button>
      ) : null}
      <Spacer y={3} />
      <CenteredBoxBottomButtons
        disableActionButton={isContinueButtonDisabled}
        onActionButton={onClickContinue}
        onBackButton={onClickBack}
        backButtonText={t("cancel")}
      />
    </>
  );
};
