import { Dispatch, SetStateAction } from "react";
import { ActionRadio, Spacer, Typography } from "@atoms";
import { CenteredBoxBottomButtons } from "@molecules";
import {
  useCreateGovernanceActionForm,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { GovernanceActionTooltip } from "@/consts";
import { GovernanceActionType } from "@/types/governanceAction";

type ChooseGovernanceActionTypeProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const ChooseGovernanceActionType = ({
  setStep,
}: ChooseGovernanceActionTypeProps) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const { getValues, setValue, watch } = useCreateGovernanceActionForm();

  const isContinueButtonDisabled = !watch("governance_action_type");

  const onClickContinue = () => {
    setStep(3);
  };

  const onClickBack = () => {
    setStep(1);
  };

  const onChangeType = (value: string) => {
    setValue("governance_action_type", value as GovernanceActionType);
  };

  const renderGovernanceActionTypes = () =>
    Object.keys(GovernanceActionType).map(
      (type, index, governanceActionTypes) => {
        const isChecked = getValues("governance_action_type") === type;
        return (
          <div key={type}>
            <ActionRadio
              isChecked={isChecked}
              dataTestId={`${type}-radio`}
              onChange={onChangeType}
              title={type}
              value={type}
              tooltipTitle={type}
              tooltipText={
                GovernanceActionTooltip[
                  type as keyof typeof GovernanceActionTooltip
                ]
              }
            />
            {index + 1 < governanceActionTypes.length ? <Spacer y={2} /> : null}
          </div>
        );
      },
    );

  return (
    <>
      <Typography sx={{ textAlign: "center" }} variant="headline4">
        {t("createGovernanceAction.chooseGATypeTitle")}
      </Typography>
      <Spacer y={isMobile ? 4.25 : 7.5} />
      {renderGovernanceActionTypes()}
      <Spacer y={isMobile ? 6 : 7.5} />
      <CenteredBoxBottomButtons
        disableActionButton={isContinueButtonDisabled}
        onActionButton={onClickContinue}
        onBackButton={onClickBack}
        backButtonText={t("cancel")}
      />
    </>
  );
};
