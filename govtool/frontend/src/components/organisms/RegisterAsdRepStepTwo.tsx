import { Dispatch, SetStateAction, useCallback, useState } from "react";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import {
  useRegisterAsdRepForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";

import { BgCard, ControlledField } from ".";

export const RegisterAsdRepStepTwo = ({
  setStep,
}: {
  setStep: Dispatch<SetStateAction<number>>;
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { control, errors } = useRegisterAsdRepForm();
  const [links, setLinks] = useState<any>([""]);

  const onClickBackButton = useCallback(() => setStep(1), []);

  const onClickContinue = useCallback(() => setStep(3), [setStep]);

  const addLink = useCallback(
    () =>
      links.length === 7 ? {} : setLinks((prev: string[]) => [...prev, ""]),
    [links]
  );

  const removeLink = useCallback(
    (clickedIndex: number) => {
      setLinks((prev: string[]) =>
        prev.filter((_, index) => clickedIndex !== index)
      );
    },
    [links]
  );

  const renderLinks = useCallback(() => {
    return links.map((_: any, index: number) => (
      <>
        <ControlledField.Input
          {...{ control, errors }}
          label={t("forms.link") + ` ${index + 1}`}
          name={"link" + (index + 1)}
          endAdornment={
            <DeleteOutlineIcon
              color="primary"
              sx={{ cursor: "pointer", height: 24, with: 24 }}
              onClick={() => removeLink(index)}
            />
          }
          placeholder={t("forms.linkPlaceholder")}
        />
        <Spacer y={3} />
      </>
    ));
  }, [control, errors, links]);

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBackButton}
      title={t("registration.becomeADRep")}
    >
      <Box textAlign="center">
        <InfoText>{t("registration.required")}</InfoText>
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("registration.dRepName")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("registration.dRepNameDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        helpfulText={t("forms.dRepNameHelpfulText")}
        label={t("forms.dRepName")}
        name="dRepName"
        placeholder={t("forms.dRepNamePlaceholder")}
      />
      <Spacer y={isMobile ? 5 : 6} />
      <Box textAlign="center">
        <InfoText>{t("registration.optional")}</InfoText>
        <Typography sx={{ mt: 0.5, mb: isMobile ? 3 : 4 }} variant="headline4">
          {t("registration.aboutYou")}
        </Typography>
        <Typography fontWeight={400} sx={{ mb: 4 }} variant="body1">
          {t("registration.aboutYouDescription")}
        </Typography>
      </Box>
      <ControlledField.Input
        {...{ control, errors }}
        label={t("forms.email")}
        name="email"
        placeholder={t("forms.emailPlaceholder")}
      />
      <Spacer y={3} />
      <ControlledField.TextArea
        {...{ control, errors }}
        label={t("forms.bio")}
        name="bio"
        placeholder={t("forms.bioPlaceholder")}
        helpfulText={t("forms.bioHelpfulText")}
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
      <Button
        onClick={addLink}
        size="extraLarge"
        sx={{ mb: isMobile ? 5 : 6 }}
        variant="text"
      >
        + Add link
      </Button>
    </BgCard>
  );
};
