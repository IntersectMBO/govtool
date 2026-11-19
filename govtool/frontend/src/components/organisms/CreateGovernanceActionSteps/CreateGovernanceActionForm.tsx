import { Dispatch, SetStateAction, useCallback } from "react";
import { Path, RegisterOptions, useFieldArray } from "react-hook-form";
import { Box, ButtonBase } from "@mui/material";
import { ChevronDown, Plus, Trash2 } from "lucide-react";

import type { TypographyProps } from "@atoms";
import { GOVERNANCE_ACTION_FIELDS, Placeholders } from "@consts";
import {
  CreateGovernanceActionValues,
  useCreateGovernanceActionForm,
  useTranslation,
} from "@hooks";
import { useFeatureFlag } from "@context";
import { CenteredBoxBottomButtons, Field } from "@molecules";
import { ds, RADIUS } from "@/consts/designTokens";
import { URL_REGEX, testIdFromLabel } from "@/utils";
import {
  GovernanceActionField,
  GovernanceActionType,
} from "@/types/governanceAction";

import { ControlledField } from "../ControlledField";
import { SectionCard, SectionTag, StepHeading } from "./parts";

const MAX_NUMBER_OF_LINKS = 8;

// Title sits under Identity and the three texts under About Action; every
// other field of a type is its action-specific detail.
const IDENTITY_FIELDS = ["title"];
const ABOUT_FIELDS = ["abstract", "motivation", "rationale"];

const LABEL_STYLES: TypographyProps = { color: ds.gray["900"], fontWeight: 500 };

type FieldsOfType = (typeof GOVERNANCE_ACTION_FIELDS)[
  | GovernanceActionType.InfoAction
  | GovernanceActionType.TreasuryWithdrawals
  | GovernanceActionType.NewCommittee
  | GovernanceActionType.NewConstitution
  | GovernanceActionType.NoConfidence
  | GovernanceActionType.HardForkInitiation
  | GovernanceActionType.ParameterChange];

type CreateGovernanceActionFormProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const CreateGovernanceActionForm = ({
  setStep,
}: CreateGovernanceActionFormProps) => {
  const { t } = useTranslation();
  const { isCip179Enabled } = useFeatureFlag();
  const { control, errors, getValues, register, reset, watch } =
    useCreateGovernanceActionForm();

  const isError = Object.keys(errors).length > 0;

  const type = getValues("governance_action_type");
  // TODO: Provide better typing for GOVERNANCE_ACTION_FIELDS
  // to get rid of explicit type assertion
  const fields = GOVERNANCE_ACTION_FIELDS[
    type! as keyof typeof GOVERNANCE_ACTION_FIELDS
  ] as FieldsOfType;
  const {
    append,
    fields: references,
    remove,
  } = useFieldArray({
    control,
    name: "references",
  });

  const isContinueButtonDisabled =
    Object.keys(fields).some(
      (field) => !watch(field as unknown as Parameters<typeof watch>[0]),
    ) || isError;

  const onClickContinue = () => {
    setStep(4);
  };

  const onClickBack = () => {
    reset();
    setStep(2);
  };

  const renderFields = (keys: string[]) =>
    Object.entries(fields)
      .filter(([key]) => keys.includes(key))
      .map(([key, field]) => {
        const fieldProps = {
          helpfulText: field.tipI18nKey ? t(field.tipI18nKey) : undefined,
          key,
          label: t(field.labelI18nKey),
          labelStyles: LABEL_STYLES,
          name: key as Path<CreateGovernanceActionValues>,
          placeholder: field.placeholderI18nKey
            ? t(field.placeholderI18nKey)
            : undefined,
          rules: field.rules as unknown as Omit<
            RegisterOptions<
              CreateGovernanceActionValues,
              Path<CreateGovernanceActionValues>
            >,
            "valueAsNumber" | "valueAsDate" | "setValueAs"
          >,
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
        return null;
      });

  const detailKeys = Object.keys(fields).filter(
    (key) => !IDENTITY_FIELDS.includes(key) && !ABOUT_FIELDS.includes(key),
  );
  const isTreasury = type === GovernanceActionType.TreasuryWithdrawals;

  const addLink = useCallback(() => append({ uri: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const renderLinks = useCallback(
    () =>
      references.map((field, index) => (
        <Box key={field.id} sx={{ alignItems: "flex-end", display: "flex", gap: 1.5 }}>
          <ControlledField.Input
            {...register(`references.${index}.uri`)}
            errors={errors}
            label={`${t("forms.link")} ${index + 1}`}
            labelStyles={LABEL_STYLES}
            layoutStyles={{ flex: 1 }}
            placeholder={Placeholders.LINK}
            name={`references.${index}.uri`}
            rules={{
              pattern: {
                value: URL_REGEX,
                message: t("createGovernanceAction.fields.validations.url"),
              },
            }}
          />
          {references.length > 1 && (
            <ButtonBase
              aria-label={t("createGA.details.removeLink", { index: index + 1 })}
              data-testid={`delete-link-${index + 1}-button`}
              onClick={() => removeLink(index)}
              sx={{
                backgroundColor: ds.error["50"],
                borderRadius: `${RADIUS.md}px`,
                color: ds.error["600"],
                flexShrink: 0,
                height: 40,
                mb: 0.25,
                width: 40,
                "&:hover": { backgroundColor: ds.error["100"] },
                "&:focus-visible": { outline: `2px solid ${ds.error["600"]}`, outlineOffset: 2 },
              }}
            >
              <Trash2 aria-hidden size={18} />
            </ButtonBase>
          )}
        </Box>
      )),
    [references],
  );

  const column = { display: "flex", flexDirection: "column", gap: 2.5 };

  return (
    <>
      <StepHeading body={t("createGA.details.body")} title={t("createGA.details.title")} />
      <SectionCard
        tag={<SectionTag label={t("createGA.details.required")} tone="error" />}
        testId="create-ga-identity"
        title={t("createGA.details.identity")}
      >
        <Box sx={column}>
          <Field.Input
            disabled
            endAdornment={<ChevronDown aria-hidden color={ds.gray["500"]} size={18} />}
            helpfulText={t("createGA.details.typeHint")}
            label={t("forms.createGovernanceAction.typeLabel")}
            labelStyles={LABEL_STYLES}
            value={type ? t(`createGA.type.names.${type}`) : ""}
          />
          {renderFields(IDENTITY_FIELDS)}
        </Box>
      </SectionCard>
      <SectionCard testId="create-ga-about" title={t("createGA.details.about")}>
        <Box sx={column}>{renderFields(ABOUT_FIELDS)}</Box>
      </SectionCard>
      {detailKeys.length > 0 && (
        <SectionCard
          subtitle={isTreasury ? t("createGA.details.addressBody") : undefined}
          testId="create-ga-parameters"
          title={isTreasury ? t("createGA.details.address") : t("createGA.details.parameters")}
        >
          <Box sx={column}>{renderFields(detailKeys)}</Box>
        </SectionCard>
      )}
      {isCip179Enabled && (
        <SectionCard
          tag={<SectionTag label={t("createGA.details.optional")} tone="neutral" />}
          testId="create-ga-survey"
          title={t("createGA.details.survey")}
        >
          <ControlledField.Input
            {...register("surveyTxId")}
            errors={errors}
            helpfulText={t("createGovernanceAction.surveyTxIdHelp")}
            label={t("createGovernanceAction.surveyTxId")}
            labelStyles={LABEL_STYLES}
            name="surveyTxId"
            placeholder={t("createGovernanceAction.surveyTxIdPlaceholder")}
            rules={{
              pattern: {
                value: /^[0-9a-fA-F]{64}$/,
                message: t("createGovernanceAction.surveyTxIdInvalid"),
              },
            }}
          />
        </SectionCard>
      )}
      <SectionCard
        tag={<SectionTag label={t("createGA.details.optional")} tone="neutral" />}
        testId="create-ga-links"
        title={t("createGA.details.links")}
      >
        <Box sx={column}>
          {renderLinks()}
          {references?.length < MAX_NUMBER_OF_LINKS ? (
            <ButtonBase
              data-testid="add-link-button"
              onClick={addLink}
              sx={{
                alignSelf: "flex-start",
                borderRadius: `${RADIUS.sm}px`,
                color: ds.primary["600"],
                fontSize: 14,
                fontWeight: 500,
                gap: 0.75,
                px: 0.5,
                py: 0.5,
                "&:hover": { color: ds.primary["700"] },
                "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
              }}
            >
              <Plus aria-hidden size={16} />
              {t("createGA.details.addLink")}
            </ButtonBase>
          ) : null}
        </Box>
      </SectionCard>
      <CenteredBoxBottomButtons
        actionButtonText={t("createGA.details.continue")}
        disableActionButton={isContinueButtonDisabled}
        onActionButton={onClickContinue}
        onBackButton={onClickBack}
        backButtonText={t("cancel")}
      />
    </>
  );
};
