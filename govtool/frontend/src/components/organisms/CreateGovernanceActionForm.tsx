import { Dispatch, SetStateAction, useCallback } from "react";
import { useFieldArray } from "react-hook-form";
import DeleteOutlineIcon from "@mui/icons-material/DeleteOutline";

import { Button, InfoText, Spacer, Typography } from "@atoms";
import { GOVERNANCE_ACTIONS_FIELDS } from "@consts";
import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { Field } from "@molecules";

import { BgCard } from "./BgCard";
import { ControlledField } from "./ControlledField";

const LINK_PLACEHOLDER = "https://website.com/";
const MAX_NUMBER_OF_LINKS = 7;

type ChooseGovernanceActionTypeProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const CreateGovernanceActionForm = ({
  setStep,
}: ChooseGovernanceActionTypeProps) => {
  const { t } = useTranslation();
  const { control, errors, getValues, register, watch } =
    useCreateGovernanceActionForm();
  const {
    append,
    fields: links,
    remove,
  } = useFieldArray({
    control,
    name: "links",
  });

  const governanceActionType = getValues("type");
  const fields =
    GOVERNANCE_ACTIONS_FIELDS.find(
      (field) => field.name === governanceActionType
    )?.fields ?? [];

  // TODO: Replace any
  const isContinueButtonDisabled = Object.keys(fields).some(
    (field: any) => !watch(field)
  );

  const onClickContinue = () => {
    setStep(4);
  };

  const onClickBack = () => {
    setStep(2);
  };

  const renderGovernanceActionField = () => {
    return Object.entries(fields).map(([key, value]) => {
      const label =
        key.charAt(0).toUpperCase() + key.slice(1).replace("_", " ");

      if (value.component === "input") {
        return (
          <ControlledField.Input
            {...{ control, errors }}
            helpfulText={value.tip}
            key={key}
            label={label}
            layoutStyles={{ mb: 3 }}
            name={key}
            placeholder={value.placeholder}
          />
        );
      }
      if (value.component === "textarea") {
        return (
          <ControlledField.TextArea
            {...{ control, errors }}
            helpfulText={value.tip}
            key={key}
            label={label}
            layoutStyles={{ mb: 3 }}
            name={key}
            placeholder={value.placeholder}
          />
        );
      }
    });
  };

  const addLink = useCallback(() => {
    append({ link: "" });
  }, [append]);

  const removeLink = useCallback(
    (index: number) => {
      remove(index);
    },
    [remove]
  );

  const renderLinks = useCallback(() => {
    return links.map((field, index) => {
      return (
        <ControlledField.Input
          {...register(`links.${index}.link`)}
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
          label={t("forms.link") + ` ${index + 1}`}
          layoutStyles={{ mb: 3 }}
          placeholder={LINK_PLACEHOLDER}
        />
      );
    });
  }, [links]);

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      isActionButtonDisabled={isContinueButtonDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickBack}
    >
      <InfoText label={t("required")} sx={{ mb: 0.75, textAlign: "center" }} />
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.formTitle")}
      </Typography>
      <Spacer y={4.25} />
      <Field.Input
        disabled={true}
        helpfulText={t("forms.createGovernanceAction.typeTip")}
        label={t("forms.createGovernanceAction.typeLabel")}
        value={governanceActionType}
      />
      <Spacer y={3} />
      {renderGovernanceActionField()}
      <InfoText label={t("optional")} sx={{ mb: 0.75, textAlign: "center" }} />
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.references")}
      </Typography>
      <Spacer y={4.25} />
      {renderLinks()}
      {links?.length < MAX_NUMBER_OF_LINKS ? (
        <Button onClick={addLink} size="extraLarge" variant="text">
          {t("addLink")}
        </Button>
      ) : null}
      <Spacer y={3} />
    </BgCard>
  );
};
