import { Dispatch, SetStateAction, useCallback } from "react";
import { useFieldArray } from "react-hook-form";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import {
  useRegisterAsdRepForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { URL_REGEX } from "@utils";

import { BgCard, ControlledField } from "..";
import { Placeholders } from "@/consts";

const MAX_NUMBER_OF_LINKS = 8;

export const RegisterAsDRepForm = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors, register, watch } = useRegisterAsdRepForm();
  const {
    append,
    fields: links,
    remove,
  } = useFieldArray({
    control,
    name: "links",
  });

  const onClickContinue = () => setStep(3);

  const onClickBack = () => setStep(1);

  const addLink = useCallback(() => append({ link: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const isContinueButtonDisabled = !watch("dRepName");

  const renderLinks = useCallback(
    () =>
      links.map((field, index) => (
        <ControlledField.Input
          {...register(`links.${index}.link`)}
          errors={errors}
          endAdornment={
            links.length > 1 ? (
              <DeleteOutlineIcon
                color="primary"
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
          name={`links.${index}.link`}
          rules={{
            pattern: {
              value: URL_REGEX,
              message: t("createGovernanceAction.fields.validations.url"),
            },
          }}
        />
      )),
    [links],
  );

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBack}
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
        helpfulText={t("forms.registerAsDRep.dRepNameHelpfulText")}
        label={t("forms.registerAsDRep.dRepName")}
        name="dRepName"
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
      />
      <Spacer y={3} />
      <ControlledField.TextArea
        {...{ control, errors }}
        label={t("forms.registerAsDRep.bio")}
        name="bio"
        placeholder={t("forms.registerAsDRep.bioPlaceholder")}
        helpfulText={t("forms.registerAsDRep.bioHelpfulText")}
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
          {t("registration.maximumLinks")}
        </span>
      </p>
      <Spacer y={3} />
      {renderLinks()}
      {links?.length < MAX_NUMBER_OF_LINKS ? (
        <Button onClick={addLink} size="extraLarge" variant="text">
          {t("addLink")}
        </Button>
      ) : null}
      <Spacer y={isMobile ? 4 : 6} />
    </BgCard>
  );
};
