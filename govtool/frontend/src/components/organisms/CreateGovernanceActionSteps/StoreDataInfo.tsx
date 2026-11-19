import { Dispatch, SetStateAction } from "react";
import { Box, ButtonBase } from "@mui/material";
import { ExternalLink } from "lucide-react";

import { useCreateGovernanceActionForm, useTranslation } from "@hooks";
import { CenteredBoxBottomButtons } from "@molecules";
import { ControlledField } from "@organisms";
import { openInNewTab } from "@utils";
import { LINKS } from "@/consts/links";
import { ds, RADIUS } from "@/consts/designTokens";

import { SectionCard, StepHeading } from "./parts";

type StoreDataInfoProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

export const StoreDataInfo = ({ setStep }: StoreDataInfoProps) => {
  const { t } = useTranslation();
  const { control, errors, watch } = useCreateGovernanceActionForm();

  const openLink = () => openInNewTab(LINKS.STORING_INFORMATION_OFFLINE);

  const isContinueDisabled = !watch("storeData");

  return (
    <>
      <StepHeading body={t("createGA.storeData.body")} title={t("createGA.storeData.title")} />
      <SectionCard testId="create-ga-store-data" title={t("createGovernanceAction.storeDataTitle")}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
          <ButtonBase
            data-testid="storing-information-link"
            onClick={openLink}
            sx={{
              alignSelf: "flex-start",
              borderRadius: `${RADIUS.sm}px`,
              color: ds.primary["600"],
              fontFamily: "inherit",
              fontSize: 15,
              fontWeight: 500,
              gap: 0.75,
              "&:hover": { textDecoration: "underline" },
              "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
            }}
          >
            {t("createGovernanceAction.storeDataLink")}
            <ExternalLink aria-hidden size={15} />
          </ButtonBase>
          <Box sx={{ borderTop: `1px solid ${ds.gray["100"]}` }} />
          <ControlledField.Checkbox
            {...{ control, errors }}
            data-testid="storing-information-checkbox"
            label={t("createGovernanceAction.storeDataCheckboxLabel")}
            labelStyles={{ color: ds.gray["800"], fontSize: 14 }}
            name="storeData"
          />
        </Box>
      </SectionCard>
      <CenteredBoxBottomButtons
        backButtonText={t("back")}
        disableActionButton={isContinueDisabled}
        onActionButton={() => setStep(6)}
        onBackButton={() => setStep(4)}
      />
    </>
  );
};
