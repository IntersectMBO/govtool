import { useState } from "react";
import { Box, ButtonBase } from "@mui/material";
import { useLocation } from "react-router";
import { useTranslation } from "react-i18next";
import { CircleCheck } from "lucide-react";

import { ds, RADIUS } from "@/consts/designTokens";

/** "You have successfully retired as a Direct Voter", after the retire page lands here. */
export const RetiredBanner = () => {
  const { t } = useTranslation();
  const { state } = useLocation();
  const [isDismissed, setIsDismissed] = useState(false);
  if (isDismissed || (state as { retired?: string } | null)?.retired !== "directVoter") return null;
  return (
    <Box
      role="status"
      sx={{ alignItems: "center", backgroundColor: ds.success["50"], border: `1px solid ${ds.success["200"]}`, borderLeft: `3px solid ${ds.success["600"]}`, borderRadius: `${RADIUS.md}px`, color: ds.success["700"], display: "flex", fontSize: 14, fontWeight: 500, gap: 1.25, px: 2, py: 1.25 }}
    >
      <CircleCheck aria-hidden size={18} style={{ flexShrink: 0 }} />
      <Box sx={{ flex: 1 }}>{t("directVoter.retiredBanner")}</Box>
      <ButtonBase
        data-testid="retired-banner-dismiss"
        onClick={() => setIsDismissed(true)}
        sx={{ borderRadius: "4px", color: ds.success["700"], fontSize: 14, fontWeight: 600, textDecoration: "underline", "&:focus-visible": { outline: `2px solid ${ds.success["700"]}` } }}
      >
        {t("directVoter.dismiss")}
      </ButtonBase>
    </Box>
  );
};
