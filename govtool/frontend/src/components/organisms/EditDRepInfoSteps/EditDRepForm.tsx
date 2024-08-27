import { Dispatch, SetStateAction, useCallback, useEffect } from "react";
import { useLocation } from "react-router-dom";
import { useFieldArray } from "react-hook-form";
import { Box } from "@mui/material";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { Placeholders, Rules } from "@consts";
import { useCardano } from "@context";
import {
  useEditDRepInfoForm,
  useGetDRepListInfiniteQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";

import { ControlledField } from "..";

const MAX_NUMBER_OF_LINKS = 7;

export const EditDRepForm = ({
  onClickCancel,
  setStep,
  loadUserData,
  setLoadUserData,
}: {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
  loadUserData: boolean;
  setLoadUserData: Dispatch<SetStateAction<boolean>>;
}) => {
  const { state } = useLocation();
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { dRepID } = useCardano();
  const { control, errors, isError, register, watch, reset } =
    useEditDRepInfoForm();
  const {
    append,
    fields: references,
    remove,
  } = useFieldArray({
    control,
    name: "references",
  });

  const { dRepData: yourselfDRepList } = useGetDRepListInfiniteQuery(
    {
      searchPhrase: dRepID,
    },
    { enabled: !state },
  );

  const onClickContinue = () => {
    setStep(2);
    setLoadUserData(false);
  };

  const addLink = useCallback(() => append({ uri: "" }), [append]);

  const removeLink = useCallback((index: number) => remove(index), [remove]);

  const isContinueButtonDisabled = !watch("givenName") || isError;

  useEffect(() => {
    if (loadUserData) {
      reset(
        state
          ? {
              ...state,
              references: state.references.length
                ? state.references.map((uri: string) => ({
                    uri,
                  }))
                : [{ uri: "" }],
            }
          : {
              ...yourselfDRepList?.[0],
              references: yourselfDRepList?.[0].references.length
                ? yourselfDRepList?.[0].references.map((uri) => ({
                    uri,
                  }))
                : [{ uri: "" }],
            },
      );
    }
  }, [yourselfDRepList?.[0], loadUserData]);

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
          maxLength={Rules.OBJECTIVES.maxLength.value}
        />
        <ControlledField.TextArea
          {...{ control, errors }}
          data-testid="motivations-input"
          label={t("forms.dRepData.motivations")}
          name="motivations"
          placeholder={t("forms.dRepData.motivationsPlaceholder")}
          helpfulText={t("forms.dRepData.motivationsHelpfulText")}
          rules={Rules.MOTIVATIONS}
          maxLength={Rules.MOTIVATIONS.maxLength.value}
        />
        <ControlledField.TextArea
          {...{ control, errors }}
          data-testid="qualifications-input"
          label={t("forms.dRepData.qualifications")}
          name="qualifications"
          placeholder={t("forms.dRepData.qualificationsPlaceholder")}
          helpfulText={t("forms.dRepData.qualificationsHelpfulText")}
          rules={Rules.QUALIFICATIONS}
          maxLength={Rules.QUALIFICATIONS.maxLength.value}
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
        <CenteredBoxBottomButtons
          onActionButton={onClickContinue}
          disableActionButton={isContinueButtonDisabled}
          onBackButton={onClickCancel}
          backButtonText={t("cancel")}
        />
      </Box>
    </>
  );
};
