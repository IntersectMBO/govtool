import { Box, ButtonBase } from "@mui/material";
import { ArrowRight } from "lucide-react";

import { useTranslation } from "@hooks";
import { FONT_FAMILY } from "@/consts/designTokens";

type Props = {
  /** Called by "Propose Action". */
  onPropose: () => void;
};

/** The orange "Have an idea for Cardano governance?" call to action. */
export const ProposeActionBanner = ({ onPropose }: Props) => {
  const { t } = useTranslation();
  const steps = [
    t("govActions.proposeBanner.stepCreate"),
    t("govActions.proposeBanner.stepReview"),
    t("govActions.proposeBanner.stepSubmit"),
  ];

  return (
    <Box
      component="section"
      data-testid="propose-action-banner"
      sx={{
        alignItems: { md: "center" },
        background:
          "radial-gradient(60% 120% at 100% 100%, rgba(255, 255, 255, 0.55) 0%, rgba(255, 255, 255, 0) 60%), linear-gradient(110deg, #E9682F 0%, #F07A3E 55%, #F4A274 100%)",
        borderRadius: "16px",
        display: "flex",
        flexDirection: { xxs: "column", md: "row" },
        gap: 3,
        justifyContent: "space-between",
        mt: 4,
        overflow: "hidden",
        p: { xxs: 3, md: 6 },
        position: "relative",
      }}
    >
      <Box
        aria-hidden
        sx={{
          backgroundImage: "repeating-linear-gradient(90deg, rgba(255, 255, 255, 0.22) 0 1px, transparent 1px 12px)",
          bottom: 0,
          display: { xxs: "none", md: "block" },
          maskImage: "linear-gradient(180deg, transparent 0%, #000 40%)",
          position: "absolute",
          right: 0,
          top: 24,
          width: 260,
        }}
      />
      <Box sx={{ maxWidth: 560, position: "relative" }}>
        <Box component="span" sx={{ backgroundColor: "#C2410C", borderRadius: "4px", color: "#FFFFFF", fontSize: 10, fontWeight: 700, lineHeight: "13px", px: 0.75, py: 0.5, textTransform: "uppercase" }}>
          {t("govActions.proposeBanner.tag")}
        </Box>
        <Box component="h2" sx={{ color: "#FFFFFF", fontFamily: FONT_FAMILY.serif, fontSize: { xxs: 28, md: 40 }, fontWeight: 400, lineHeight: { xxs: "34px", md: "46px" }, m: 0, mt: 1.5 }}>
          {t("govActions.proposeBanner.title")}
        </Box>
        <Box component="p" sx={{ alignItems: "center", color: "#FFFFFF", display: "flex", flexWrap: "wrap", fontSize: 14, gap: 1, lineHeight: "20px", m: 0, mt: 2 }}>
          {steps.map((step, position) => (
            <Box key={step} component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 1 }}>
              {position > 0 && <ArrowRight aria-hidden size={14} />}
              {step}
            </Box>
          ))}
        </Box>
        <Box component="p" sx={{ color: "rgba(255, 255, 255, 0.92)", fontSize: 14, lineHeight: "20px", m: 0, mt: 1 }}>
          {t("govActions.proposeBanner.description")}
        </Box>
      </Box>
      <ButtonBase
        data-testid="propose-action-banner-button"
        onClick={onPropose}
        sx={{
          alignItems: "center",
          alignSelf: { xxs: "flex-start", md: "center" },
          backgroundColor: "#FFFFFF",
          borderRadius: "999px",
          boxShadow: "0px 22px 22px 9px rgba(233, 104, 47, 0.12)",
          color: "#E9682F",
          display: "inline-flex",
          flexShrink: 0,
          fontFamily: "inherit",
          fontSize: 16,
          fontWeight: 600,
          gap: 1,
          height: 48,
          letterSpacing: "-0.18px",
          position: "relative",
          px: 2.5,
          textDecoration: "none",
          "&.Mui-focusVisible": { outline: "2px solid #FFFFFF", outlineOffset: 2 },
        }}
      >
        {t("govActions.proposeBanner.cta")}
        <ArrowRight aria-hidden size={18} strokeWidth={2} />
      </ButtonBase>
    </Box>
  );
};
