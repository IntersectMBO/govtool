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

import { BgCard, ControlledField } from "..";

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

  const addLink = useCallback(() => append({ link: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const isContinueButtonDisabled = !watch("dRepName") || isError;

  const renderLinks = useCallback(
    () =>
      references.map((field, index) => (
        <ControlledField.Input
          {...register(`references.${index}.link`)}
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
          name={`references.${index}.link`}
          rules={Rules.LINK}
        />
      )),
    [errors, references],
  );

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={voter?.wasRegisteredAsDRep ? t("cancel") : t("back")}
      onClickActionButton={onClickContinue}
      onClickBackButton={
        voter?.wasRegisteredAsDRep ? onClickCancel : onClickBack
      }
      isActionButtonDisabled={isContinueButtonDisabled}
      sx={{ pb: isMobile ? undefined : 6, pt: isMobile ? 4 : 8 }}
    >
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
        helpfulText={t("forms.registerAsDRep.dRepNameHelpfulText")}
        label={t("forms.registerAsDRep.dRepName")}
        name="dRepName"
        rules={Rules.DREP_NAME}
        placeholder={t("forms.registerAsDRep.dRepNamePlaceholder")}
      />
      <Spacer y={isMobile ? 5 : 6} />
      <Box textAlign="center">
        <InfoText label={t("registration.optional")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("registration.aboutYou")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("registration.aboutYouDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        label={t("forms.registerAsDRep.email")}
        name="email"
        placeholder={t("forms.registerAsDRep.emailPlaceholder")}
        rules={Rules.EMAIL}
      />
      <Spacer y={3} />
      <ControlledField.TextArea
        {...{ control, errors }}
        data-testid="bio-input"
        label={t("forms.registerAsDRep.bio")}
        name="bio"
        placeholder={t("forms.registerAsDRep.bioPlaceholder")}
        helpfulText={t("forms.registerAsDRep.bioHelpfulText")}
        rules={Rules.BIO}
      />
      <Spacer y={4} />
      <p
        style={{
          fontFamily: "Poppins",
          fontSize: 16,
          fontWeight: 600,
          textAlign: "center",
          margin: 0,
        }}
      >
        {t("registration.linksDescription")}
        <span style={{ fontSize: 16, fontWeight: 400 }}>
          {t("registration.maximumLinks", {
            numberOfLinks: MAX_NUMBER_OF_LINKS,
          })}
        </span>
      </p>
      <Spacer y={3} />
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
      <Spacer y={isMobile ? 4 : 6} />
    </BgCard>
  );
};
