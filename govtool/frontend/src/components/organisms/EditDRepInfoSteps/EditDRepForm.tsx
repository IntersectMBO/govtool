import { Dispatch, SetStateAction, useCallback } from "react";
import { useFieldArray } from "react-hook-form";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { Placeholders, Rules } from "@consts";
import {
  useEditDRepInfoForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";

import { BgCard, ControlledField } from "..";

const MAX_NUMBER_OF_LINKS = 7;

export const EditDRepForm = ({
  onClickCancel,
  setStep,
}: {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors, isError, register, watch } = useEditDRepInfoForm();
  const {
    append,
    fields: links,
    remove,
  } = useFieldArray({
    control,
    name: "links",
  });

  const onClickContinue = () => setStep(2);

  const addLink = useCallback(() => append({ link: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const isContinueButtonDisabled = !watch("dRepName") || isError;
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
          name={`links.${index}.link`}
          rules={Rules.LINK}
        />
      )),
    [errors, links],
  );

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("cancel")}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickCancel}
      isActionButtonDisabled={isContinueButtonDisabled}
      sx={{ pb: isMobile ? undefined : 6, pt: isMobile ? 4 : 8 }}
    >
      <Box textAlign="center">
        <InfoText label={t("editMetadata.required")} />
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("editMetadata.dRepName")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("editMetadata.dRepNameDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        helpfulText={t("forms.editMetadata.dRepNameHelpfulText")}
        label={t("forms.editMetadata.dRepName")}
        name="dRepName"
        rules={Rules.DREP_NAME}
        placeholder={t("forms.editMetadata.dRepNamePlaceholder")}
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
      <ControlledField.Input
        {...{ control, errors }}
        label={t("forms.editMetadata.email")}
        name="email"
        placeholder={t("forms.editMetadata.emailPlaceholder")}
        rules={Rules.EMAIL}
      />
      <Spacer y={3} />
      <ControlledField.TextArea
        {...{ control, errors }}
        data-testid="bio-input"
        label={t("forms.editMetadata.bio")}
        name="bio"
        placeholder={t("forms.editMetadata.bioPlaceholder")}
        helpfulText={t("forms.editMetadata.bioHelpfulText")}
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
        {t("editMetadata.linksDescription")}
        <span style={{ fontSize: 16, fontWeight: 400 }}>
          {t("editMetadata.maximumLinks", {
            numberOfLinks: MAX_NUMBER_OF_LINKS,
          })}
        </span>
      </p>
      <Spacer y={3} />
      {renderLinks()}
      {links?.length < MAX_NUMBER_OF_LINKS ? (
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
