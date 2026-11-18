import { Box, ButtonBase } from "@mui/material";
import { useTranslation } from "react-i18next";
import { CircleAlert, Info, TriangleAlert } from "lucide-react";

import { ds, RADIUS } from "@/consts/designTokens";

const halfButton = {
  borderRadius: `${RADIUS.pill}px`,
  flex: 1,
  fontSize: 15,
  fontWeight: 600,
  height: 44,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

/** The first page a returning DRep sees when registering again. */
export const ReRegisterIntro = ({
  onCancel,
  onContinue,
}: {
  onCancel: () => void;
  onContinue: () => void;
}) => {
  const { t } = useTranslation();
  return (
    <Box data-testid="re-register-intro" sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["50"], borderRadius: "50%", color: ds.primary["600"], display: "flex", height: 56, justifyContent: "center", width: 56 }}>
        <TriangleAlert size={24} />
      </Box>
      <Box>
        <Box component="h1" sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, m: 0 }}>
          {t("drepDashboard.reRegister.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1.5 }}>
          {t("drepDashboard.reRegister.body")}
        </Box>
      </Box>
      <Box component="section" sx={{ backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.5, p: 2.5 }}>
        <Info aria-hidden color={ds.gray["800"]} size={18} style={{ flexShrink: 0, marginTop: 1 }} />
        <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
          <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, m: 0 }}>{t("drepDashboard.reRegister.nextTitle")}</Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t("drepDashboard.reRegister.next1")}</Box>
          <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t("drepDashboard.reRegister.next2")}</Box>
        </Box>
      </Box>
      <Box component="section" sx={{ backgroundColor: ds.warning["50"], border: `1px solid ${ds.warning["100"]}`, borderRadius: `${RADIUS.lg}px`, display: "flex", gap: 1.5, p: 2.5 }}>
        <CircleAlert aria-hidden color={ds.warning["700"]} size={18} style={{ flexShrink: 0, marginTop: 2 }} />
        <Box sx={{ display: "flex", flexDirection: "column", gap: 1 }}>
          <Box component="h2" sx={{ color: ds.warning["700"], fontSize: 16, fontWeight: 600, m: 0 }}>{t("drepDashboard.reRegister.beforeTitle")}</Box>
          <Box component="p" sx={{ color: ds.gray["700"], fontSize: 13, lineHeight: "20px", m: 0 }}>{t("drepDashboard.reRegister.before")}</Box>
        </Box>
      </Box>
      <Box aria-hidden sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
      <Box sx={{ display: "flex", flexDirection: { xxs: "column-reverse", sm: "row" }, gap: 1.5 }}>
        <ButtonBase
          data-testid="cancel-register-again-button"
          onClick={onCancel}
          sx={{ ...halfButton, backgroundColor: ds.gray["100"], border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"] }}
        >
          {t("drepDashboard.reRegister.cancel")}
        </ButtonBase>
        <ButtonBase
          data-testid="continue-register-again-button"
          onClick={onContinue}
          sx={{ ...halfButton, backgroundColor: ds.primary["600"], color: "#FFFFFF", "&:hover": { backgroundColor: ds.primary["700"] } }}
        >
          {t("drepDashboard.reRegister.continue")}
        </ButtonBase>
      </Box>
    </Box>
  );
};
