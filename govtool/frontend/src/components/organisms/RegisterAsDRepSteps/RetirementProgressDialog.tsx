import { Box, ButtonBase, CircularProgress, Dialog } from "@mui/material";
import { useTranslation } from "react-i18next";
import { X } from "lucide-react";

import { LINKS } from "@/consts/links";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { openInNewTab } from "@/utils/openInNewTab";

const halfButton = {
  borderRadius: `${RADIUS.pill}px`,
  flex: 1,
  fontSize: 15,
  fontWeight: 600,
  height: 44,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

/** A submitted retirement (DRep by default), waiting for Cardano to confirm it. */
export const RetirementProgressDialog = ({
  txHash,
  name,
  deposit,
  onClose,
  title,
  bodyText,
  showActions = true,
}: {
  txHash: string;
  /** Overrides the DRep wording, e.g. for a direct voter. */
  title?: string;
  bodyText?: string;
  showActions?: boolean;
  name?: string | null;
  /** ADA, formatted. */
  deposit?: string;
  onClose: () => void;
}) => {
  const { t } = useTranslation();
  const { cExplorerBaseUrl } = useAppContext();
  let body = bodyText ?? t("retirement.progress.bodyUnknown");
  if (deposit && !bodyText) {
    body = name
      ? t("retirement.progress.body", { name, deposit })
      : t("retirement.progress.bodyNoName", { deposit });
  }
  return (
    <Dialog
      aria-labelledby="retirement-progress-title"
      open
      PaperProps={{ sx: { borderRadius: `${RADIUS.xl}px`, maxWidth: 500, width: "100%" } }}
    >
      <Box data-testid="retirement-transaction-submitted-modal" sx={{ alignItems: "center", display: "flex", flexDirection: "column", gap: 1.5, p: { xxs: 3, md: 4 }, position: "relative", textAlign: "center" }}>
        <ButtonBase
          aria-label={t("retirement.progress.close")}
          data-testid="confirm-modal-button"
          onClick={onClose}
          sx={{ backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["700"], height: 32, position: "absolute", right: 16, top: 16, width: 32, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
        >
          <X aria-hidden size={16} />
        </ButtonBase>
        <CircularProgress aria-hidden size={36} sx={{ color: ds.orange["500"], mt: 1 }} />
        <Box role="status" sx={{ color: ds.primary["600"], fontSize: 14, fontWeight: 500 }}>{t("retirement.progress.confirming")}</Box>
        <Box component="h2" id="retirement-progress-title" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, m: 0 }}>
          {title ?? t("retirement.progress.title")}
        </Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{body}</Box>
        {showActions && <Box aria-hidden sx={{ alignSelf: "stretch", borderTop: `1px dashed ${ds.gray["200"]}`, my: 1 }} />}
        {showActions && (
        <Box sx={{ alignSelf: "stretch", display: "flex", gap: 1.5 }}>
          <ButtonBase
            onClick={() => openInNewTab(LINKS.RETIRE_AS_A_DREP)}
            sx={{ ...halfButton, backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"] }}
          >
            {t("retirement.progress.learnMore")}
          </ButtonBase>
          <ButtonBase
            data-testid="retirement-show-transaction"
            onClick={() => openInNewTab(`${cExplorerBaseUrl}/tx/${encodeURIComponent(txHash)}`)}
            sx={{ ...halfButton, backgroundColor: ds.primary["600"], color: "#FFFFFF", "&:hover": { backgroundColor: ds.primary["700"] } }}
          >
            {t("retirement.progress.showTx")}
          </ButtonBase>
        </Box>
        )}
      </Box>
    </Dialog>
  );
};
