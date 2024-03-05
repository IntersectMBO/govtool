import { Dispatch, SetStateAction } from "react";

import { ActionRadio, Spacer, Typography } from "@atoms";
import { GOVERNANCE_ACTION_TYPES } from "@consts";
import {
  useCreateGovernanceActionForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";

import { BgCard } from "./BgCard";

type ChooseGovernanceActionTypeProps = {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
};

export const ChooseGovernanceActionType = ({
  onClickCancel,
  setStep,
}: ChooseGovernanceActionTypeProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { getValues, setValue, watch } = useCreateGovernanceActionForm();

  const isContinueButtonDisabled = !watch("type");

  const onClickContinue = () => {
    setStep(2);
  };

  // TODO: Add tooltips when they will be available
  const renderGovernanceActionTypes = () => {
    return GOVERNANCE_ACTION_TYPES.map((type, index) => {
      const isChecked = getValues("type") === type;
      return (
        <div key={type}>
          <ActionRadio
            isChecked={isChecked}
            onChange={onChangeType}
            title={type}
            value={type}
          />
          {index + 1 < GOVERNANCE_ACTION_TYPES.length ? <Spacer y={2} /> : null}
        </div>
      );
    });
  };

  const onChangeType = (value: string) => {
    setValue("type", value);
  };

  return (
    <BgCard
      actionButtonLabel={t("continue")}
      backButtonLabel={t("cancel")}
      isActionButtonDisabled={isContinueButtonDisabled}
      onClickActionButton={onClickContinue}
      onClickBackButton={onClickCancel}
    >
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.chooseGATypeTitle")}
      </Typography>
      <Spacer y={isMobile ? 4.25 : 7.5} />
      {renderGovernanceActionTypes()}
      <Spacer y={isMobile ? 6 : 7.5} />
    </BgCard>
  );
};
