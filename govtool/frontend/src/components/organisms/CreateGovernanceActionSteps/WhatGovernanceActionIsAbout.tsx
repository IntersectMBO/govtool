import { useCallback, Dispatch, SetStateAction } from "react";
import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";
import { Check, CircleHelp } from "lucide-react";

import { CenteredBoxBottomButtons } from "@/components/molecules/CenteredBoxBottomButtons";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { correctVoteAdaFormat } from "@/utils/adaFormat";

import { InfoNote, SectionCard, StepHeading } from "./parts";

type WhatGovernanceActionIsAboutProps = {
  onClickCancel: () => void;
  setStep: Dispatch<SetStateAction<number>>;
};

export const WhatGovernanceActionIsAbout = ({
  onClickCancel,
  setStep,
}: WhatGovernanceActionIsAboutProps) => {
  const { t } = useTranslation();
  const { epochParams } = useAppContext();
  const onClickContinue = useCallback(() => setStep(2), []);

  const deposit = epochParams?.gov_action_deposit
    ? correctVoteAdaFormat(epochParams.gov_action_deposit)
    : undefined;
  const epochs = epochParams?.gov_action_lifetime ?? undefined;
  const points = [
    t("createGA.intro.points.provide"),
    t("createGA.intro.points.store"),
    deposit ? t("createGA.intro.points.deposit", { deposit }) : t("createGA.intro.points.depositNoAmount"),
    t("createGA.intro.points.returned"),
  ];

  return (
    <>
      <StepHeading body={t("createGA.intro.body")} title={t("createGA.intro.title")} />
      <SectionCard testId="create-ga-acknowledgement">
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], display: "flex", height: 44, justifyContent: "center", mb: 2.5, width: 44 }}>
          <CircleHelp size={22} />
        </Box>
        <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("createGA.intro.ackTitle")}</Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, m: 0, mt: 0.5 }}>{t("createGA.intro.ackBody")}</Box>
        <Box component="ul" sx={{ display: "flex", flexDirection: "column", gap: 1.5, listStyle: "none", m: 0, mt: 2.5, p: 0 }}>
          {points.map((point) => (
            <Box key={point} component="li" sx={{ alignItems: "flex-start", color: ds.gray["900"], display: "flex", fontSize: 15, gap: 1.25, lineHeight: "22px" }}>
              <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: "50%", color: ds.primary["600"], display: "flex", flexShrink: 0, height: 22, justifyContent: "center", width: 22 }}>
                <Check size={14} strokeWidth={2.5} />
              </Box>
              {point}
            </Box>
          ))}
        </Box>
      </SectionCard>
      <InfoNote>
        {epochs ? t("createGA.intro.info", { epochs }) : t("createGA.intro.infoNoEpochs")}
      </InfoNote>
      <CenteredBoxBottomButtons
        actionButtonText={t("createGA.intro.accept")}
        backButtonText={t("cancel")}
        onActionButton={onClickContinue}
        onBackButton={onClickCancel}
      />
    </>
  );
};
