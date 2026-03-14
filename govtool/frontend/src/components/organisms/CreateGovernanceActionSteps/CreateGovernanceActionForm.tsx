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
  const surveyTxId = watch("surveyTxId") ?? "";

  const validateSurveyTxId = useCallback(
    (value?: string) => {
      if (!attachSurvey) {
        return true;
      }

      if (!value?.trim()) {
        return t("createGovernanceAction.fields.validations.required");
      }

      if (!/^[0-9a-fA-F]{64}$/.test(value.trim())) {
        return "Survey transaction id must be a 64-character hex string.";
      }

      return true;
    },
    [attachSurvey, t],
  );

  const isSurveySectionEnabled = !!attachSurvey;
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
    (isSurveySectionEnabled && !surveyTxId.trim()) ||
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
      <Spacer y={1} />
      <InfoText label={t("optional")} sx={{ mb: 0.75, textAlign: "center" }} />
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        Survey Link
      </Typography>
      <Spacer y={2.5} />
      <ControlledField.Checkbox
        {...{ control, errors }}
        data-testid="attach-survey-checkbox"
        label="Link an existing on-chain survey"
        name="attachSurvey"
      />
      {attachSurvey && (
        <>
          <Spacer y={2} />
          <ControlledField.Input
            {...{ control, errors }}
            data-testid="survey-tx-id-input"
            helpfulText="Paste the existing survey transaction id from metadata label 17."
            label="Survey transaction id"
            layoutStyles={{ mb: 3 }}
            name="surveyTxId"
            placeholder="aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
            rules={{
              validate: validateSurveyTxId,
            }}
          />
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
