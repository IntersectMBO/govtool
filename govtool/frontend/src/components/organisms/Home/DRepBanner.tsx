import { Box } from "@mui/material";
import { ArrowRight } from "lucide-react";

import { useTranslation } from "@hooks";
import { ds } from "@/consts/designTokens";
import { LINKS } from "@/consts/links";

type DRepBannerProps = {
  /** False inside a page that already has the 1180px column and its gutters. */
  withGutters?: boolean;
};

/** The blue "What is a DRep?" call to action, linking to the DRep guide. */
export const DRepBanner = ({ withGutters = true }: DRepBannerProps) => {
  const { t } = useTranslation();

  return (
    <Box sx={{ boxSizing: "border-box", maxWidth: 1180, mx: "auto", px: withGutters ? { xxs: 2, md: 3, xl: 0 } : 0, width: "100%" }}>
      <Box
        data-testid="drep-banner"
        sx={{
          alignItems: { md: "center" },
          background: `radial-gradient(40% 70% at 75% 115%, #FCE9DC 0%, rgba(252, 233, 220, 0) 100%), radial-gradient(35% 60% at 0% 0%, rgba(255, 255, 255, 0.35) 0%, rgba(255, 255, 255, 0) 100%), linear-gradient(110deg, ${ds.primary["700"]} 0%, ${ds.primary["600"]} 60%, #3D6BF0 100%)`,
          borderRadius: "16px",
          display: "flex",
          flexDirection: { xxs: "column", md: "row" },
          gap: 3,
          justifyContent: "space-between",
          overflow: "hidden",
          p: { xxs: 3, md: 6 },
          position: "relative",
        }}
      >
        <Box
          aria-hidden
          sx={{
            backgroundImage: "repeating-linear-gradient(90deg, rgba(255, 255, 255, 0.18) 0 1px, transparent 1px 12px)",
            bottom: 0,
            display: { xxs: "none", md: "block" },
            maskImage: "linear-gradient(180deg, transparent 0%, #000 40%)",
            position: "absolute",
            right: 0,
            top: 24,
            width: 220,
          }}
        />
        <Box sx={{ maxWidth: 640, position: "relative" }}>
          <Box component="span" sx={{ backgroundColor: ds.primary["800"], borderRadius: "4px", color: "#FFFFFF", fontSize: 10, fontWeight: 700, lineHeight: "13px", px: 0.75, py: 0.5, textTransform: "uppercase" }}>
            {t("landing.drepBanner.tag")}
          </Box>
          <Box component="h2" sx={{ color: "#FFFFFF", fontSize: { xxs: 28, md: 36 }, fontWeight: 700, letterSpacing: "-0.5px", lineHeight: { xxs: "34px", md: "40px" }, m: 0, mt: 1.5 }}>
            {t("landing.drepBanner.title")}
          </Box>
          <Box component="p" sx={{ color: "rgba(255, 255, 255, 0.9)", fontSize: 14, lineHeight: "20px", m: 0, mt: 2 }}>
            {t("landing.drepBanner.description")}
          </Box>
        </Box>
        <Box
          component="a"
          data-testid="drep-banner-learn-more"
          href={LINKS.DREPS}
          rel="noopener noreferrer"
          target="_blank"
          sx={{
            alignItems: "center",
            alignSelf: { xxs: "flex-start", md: "center" },
            backgroundColor: "#FFFFFF",
            border: `2px solid ${ds.gray["100"]}`,
            borderRadius: "999px",
            boxShadow: "0px 22px 22px 9px rgba(20, 71, 230, 0.1)",
            color: ds.primary["600"],
            display: "inline-flex",
            flexShrink: 0,
            fontSize: 16,
            fontWeight: 600,
            gap: 1,
            height: 48,
            letterSpacing: "-0.18px",
            position: "relative",
            px: 2.5,
            textDecoration: "none",
          }}
        >
          {t("landing.drepBanner.cta")}
          <ArrowRight aria-hidden size={18} strokeWidth={2} />
        </Box>
      </Box>
    </Box>
  );
};
