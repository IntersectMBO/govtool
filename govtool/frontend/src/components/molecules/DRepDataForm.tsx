import { Fragment, useCallback } from "react";
import {
  Control,
  FieldErrors,
  UseFormRegister,
  UseFormWatch,
  useFieldArray,
} from "react-hook-form";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { Rules } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { DRepDataFormValues } from "@/types/dRep";

import { ControlledField, UncontrolledImageInput } from "../organisms";

const MAX_NUMBER_OF_LINKS = 7;

type Props = {
  control: Control<DRepDataFormValues>;
  errors: FieldErrors<DRepDataFormValues>;
  register: UseFormRegister<DRepDataFormValues>;
  watch: UseFormWatch<DRepDataFormValues>;
};

export const DRepDataForm = ({ control, errors, register, watch }: Props) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();

  return (
    <div>
      <Box textAlign="center">
        <InfoText label={t("dRepData.required")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("dRepData.dRepName")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("dRepData.dRepNameDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        dataTestId="name-input"
        label={t("forms.dRepData.givenName")}
        name="givenName"
        helpfulText={t("forms.dRepData.givenNameHelpfulText")}
        rules={Rules.GIVEN_NAME}
      />
      <Spacer y={isMobile ? 5 : 6} />
      <Box textAlign="center">
        <InfoText label={t("dRepData.optional")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("dRepData.aboutYou")}
        </Typography>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 5 }}>
        <div>
          <FieldDescription
            title={t("forms.dRepData.objectives")}
            subtitle={t("forms.dRepData.objectivesHelpfulText")}
          />
          <ControlledField.TextArea
            {...{ control, errors }}
            data-testid="objectives-input"
            label={t("forms.dRepData.objectives")}
            hideLabel
            name="objectives"
            rules={Rules.OBJECTIVES}
            maxLength={Rules.OBJECTIVES.maxLength.value}
          />
        </div>
        <div>
          <FieldDescription
            title={t("forms.dRepData.motivations")}
            subtitle={t("forms.dRepData.motivationsHelpfulText")}
          />
          <ControlledField.TextArea
            {...{ control, errors }}
            data-testid="motivations-input"
            label={t("forms.dRepData.motivations")}
            hideLabel
            name="motivations"
            rules={Rules.MOTIVATIONS}
            maxLength={Rules.MOTIVATIONS.maxLength.value}
          />
        </div>
        <div>
          <FieldDescription
            title={t("forms.dRepData.qualifications")}
            subtitle={t("forms.dRepData.qualificationsHelpfulText")}
          />
          <ControlledField.TextArea
            {...{ control, errors }}
            data-testid="qualifications-input"
            label={t("forms.dRepData.qualifications")}
            hideLabel
            name="qualifications"
            rules={Rules.QUALIFICATIONS}
            maxLength={Rules.QUALIFICATIONS.maxLength.value}
          />
        </div>
        <div>
          <FieldDescription
            title={t("forms.dRepData.image")}
            subtitle={t("forms.dRepData.imageHelpfulText")}
          />
          <UncontrolledImageInput
            data-testid="image-input"
            control={control}
            name="image"
            rules={Rules.IMAGE_URL}
          />
        </div>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 4, mt: 3 }}>
          <Typography
            variant="title2"
            fontWeight={600}
            sx={{ textAlign: "center" }}
          >
            {t("forms.dRepData.references")}
          </Typography>
          <ReferencesSection
            type="link"
            control={control}
            errors={errors}
            register={register}
            watch={watch}
          />
          <ReferencesSection
            type="identity"
            control={control}
            errors={errors}
            register={register}
            watch={watch}
          />
        </Box>
        <div>
          <Typography
            variant="title2"
            fontWeight={600}
            sx={{ textAlign: "center", mb: 1.5 }}
          >
            {t("forms.dRepData.paymentAddress")}
          </Typography>
          <Typography
            variant="body2"
            fontWeight={400}
            sx={{ textAlign: "center", mb: 0 }}
          >
            {t("forms.dRepData.paymentAddressHelpfulText")}
          </Typography>
        </div>
        <ControlledField.Input
          {...{ control, errors }}
          label={t("forms.dRepData.paymentAddress")}
          name="paymentAddress"
          rules={Rules.PAYMENT_ADDRESS}
        />
        <ControlledField.Checkbox
          {...{ control, errors }}
          label={t("forms.dRepData.doNotList")}
          labelStyles={{ fontSize: 16, fontWeight: 500 }}
          helpfulText={t("forms.dRepData.doNotListHelpfulText")}
          layoutStyles={{ ml: -1.5 }}
          name="doNotList"
          size="large"
        />
      </Box>
    </div>
  );
};

type FieldDescriptionProps = {
  title: string;
  subtitle: string;
};

const FieldDescription = ({ title, subtitle }: FieldDescriptionProps) => (
  <div>
    <Typography variant="body1" sx={{ mb: 0.5 }}>
      {title}
    </Typography>
    <Typography variant="body2" fontWeight={400} sx={{ mb: 1.5 }}>
      {subtitle}
    </Typography>
  </div>
);

type ReferencesSectionProps = {
  type: "link" | "identity";
  control: Control<DRepDataFormValues>;
  errors: FieldErrors<DRepDataFormValues>;
  register: UseFormRegister<DRepDataFormValues>;
  watch: UseFormWatch<DRepDataFormValues>;
};

const ReferencesSection = ({
  type,
  control,
  errors,
  register,
  watch,
}: ReferencesSectionProps) => {
  const { fieldName, jsonldType } = (() => {
    // eslint-disable-next-line default-case
    switch (type) {
      case "link":
        return {
          fieldName: "linkReferences",
          jsonldType: "Link",
        } as const;
      case "identity":
        return {
          fieldName: "identityReferences",
          jsonldType: "Identity",
        } as const;
    }
  })();

  const { t } = useTranslation();

  const {
    append,
    fields: references,
    remove,
  } = useFieldArray({
    control,
    name: fieldName,
  });

  const addLink = useCallback(
    () => append(getEmptyReference(jsonldType)),
    [append],
  );

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <FieldDescription
        title={t(`forms.dRepData.referenceTypes.${type}.title`)}
        subtitle={t(`forms.dRepData.referenceTypes.${type}.description`)}
      />
      {references.map((field, index) => (
        <Fragment key={field.id}>
          <ControlledField.Input
            {...register(`${fieldName}.${index}.label`)}
            errors={errors}
            label={t("forms.dRepData.referenceDescription")}
            name={`${fieldName}.${index}.label`}
            helpfulText={t("forms.dRepData.referenceDescriptionHelpfulText")}
            dataTestId={`${type}-reference-description-${index + 1}-input`}
            errorDataTestId={`${type}-reference-description-${index + 1}-error`}
            helpfulTextDataTestId={`${type}-reference-description-${
              index + 1
            }-hint`}
            rules={{
              ...Rules.LINK_DESCRIPTION,
              validate: (value) => {
                const isLink = watch(`${fieldName}.${index}.uri`);
                if (!value && Boolean(isLink)) {
                  return t("dRepData.required");
                }
                return true;
              },
            }}
          />
          <ControlledField.Input
            {...register(`${fieldName}.${index}.uri`)}
            errors={errors}
            endAdornment={
              references.length > 1 ? (
                <DeleteOutlineIcon
                  color="primary"
                  data-testid={`delete-link-${index + 1}-button`}
                  sx={{ cursor: "pointer", height: 24, with: 24 }}
                  onClick={() => removeLink(index)}
                />
              ) : null
            }
            label={t("forms.dRepData.referenceURL")}
            layoutStyles={{ mb: 3 }}
            name={`${fieldName}.${index}.uri`}
            dataTestId={`${type}-reference-url-${index + 1}-input`}
            errorDataTestId={`${type}-reference-url-${index + 1}-error`}
            helpfulTextDataTestId={`${type}-reference-url-${index + 1}-hint`}
            rules={{
              ...Rules.LINK_URL,
              validate: (value) => {
                const isDescription = watch(`${fieldName}.${index}.label`);
                if (!value && Boolean(isDescription)) {
                  return t("dRepData.required");
                }
                return true;
              },
            }}
          />
        </Fragment>
      ))}
      {references?.length < MAX_NUMBER_OF_LINKS ? (
        <Button
          data-testid={`add-${type}-reference-button`}
          onClick={addLink}
          size="extraLarge"
          variant="text"
        >
          {t("addLink")}
        </Button>
      ) : null}
    </Box>
  );
};

const getEmptyReference = (type: "Link" | "Identity") => ({
  "@type": type,
  uri: "",
  label: "",
});
