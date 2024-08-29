import { Dispatch, SetStateAction, useCallback } from "react";
import { useFieldArray } from "react-hook-form";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { Placeholders, Rules } from "@consts";
import {
  useRegisterAsdRepForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { VoterInfo } from "@models";
import { CenteredBoxBottomButtons } from "@molecules";

import { ControlledField } from "..";

const MAX_NUMBER_OF_LINKS = 7;

export const RegisterAsDRepForm = ({
  onClickCancel,
  setStep,
  voter,
}: {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
  voter?: VoterInfo;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors, isError, register, watch } = useRegisterAsdRepForm();
  const {
    append,
    fields: references,
    remove,
  } = useFieldArray({
    control,
    name: "references",
  });

  const onClickContinue = () => setStep(3);

  const onClickBack = () => setStep(1);

  const addLink = useCallback(() => append({ uri: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const isContinueButtonDisabled = !watch("givenName") || isError;

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
                sx={{ cursor: "pointer", height: 24, with: 24 }}
                onClick={() => removeLink(index)}
              />
            ) : null
          }
          key={field.id}
          // prefer-template rule for that label makes no sense
          // eslint-disable-next-line prefer-template
          label={t("forms.link") + ` ${index + 1}`}
          layoutStyles={{ mb: 3 }}
          placeholder={Placeholders.LINK}
          name={`references.${index}.uri`}
          rules={Rules.LINK}
        />
      )),
    [errors, references],
  );

  return (
    <>
      <Box textAlign="center">
        <InfoText label={t("registration.required")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("registration.dRepName")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("registration.dRepNameDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        dataTestId="name-input"
        label={t("forms.dRepData.givenName")}
        name="givenName"
        rules={Rules.GIVEN_NAME}
      />
      <Spacer y={isMobile ? 5 : 6} />
      <Box textAlign="center">
        <InfoText label={t("editMetadata.optional")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("editMetadata.aboutYou")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("editMetadata.aboutYouDescription")}
        </Typography>
      </Box>
      <Box sx={{ display: "flex", flexDirection: "column", gap: 4 }}>
        <ControlledField.TextArea
          {...{ control, errors }}
          data-testid="objectives-input"
          label={t("forms.dRepData.objectives")}
          name="objectives"
          placeholder={t("forms.dRepData.objectivesPlaceholder")}
          helpfulText={t("forms.dRepData.objectivesHelpfulText")}
          rules={Rules.OBJECTIVES}
        />
        <ControlledField.TextArea
          {...{ control, errors }}
          data-testid="motivations-input"
          label={t("forms.dRepData.motivations")}
          name="motivations"
          placeholder={t("forms.dRepData.motivationsPlaceholder")}
          helpfulText={t("forms.dRepData.motivationsHelpfulText")}
          rules={Rules.MOTIVATIONS}
        />
        <ControlledField.TextArea
          {...{ control, errors }}
          data-testid="qualifications-input"
          label={t("forms.dRepData.qualifications")}
          name="qualifications"
          placeholder={t("forms.dRepData.qualificationsPlaceholder")}
          helpfulText={t("forms.dRepData.qualificationsHelpfulText")}
          rules={Rules.QUALIFICATIONS}
        />
        <ControlledField.Input
          {...{ control, errors }}
          label={t("forms.dRepData.paymentAddress")}
          name="paymentAddress"
          placeholder={t("forms.dRepData.paymentAddressPlaceholder")}
          rules={Rules.PAYMENT_ADDRESS}
        />
        <Box sx={{ display: "flex", flexDirection: "column" }}>
          <Typography sx={{ my: 2 }}>
            {t("editMetadata.linksDescription")}
            <span style={{ fontWeight: 400 }}>
              {t("editMetadata.maximumLinks", {
                numberOfLinks: MAX_NUMBER_OF_LINKS,
              })}
            </span>
          </Typography>
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
        </Box>
      </Box>
      <CenteredBoxBottomButtons
        onActionButton={onClickContinue}
        disableActionButton={isContinueButtonDisabled}
        onBackButton={voter?.wasRegisteredAsDRep ? onClickCancel : onClickBack}
        backButtonText={voter?.wasRegisteredAsDRep ? t("cancel") : t("back")}
      />
    </>
  );
};
