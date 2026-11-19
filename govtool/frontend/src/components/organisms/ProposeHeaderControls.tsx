import { ReactNode } from "react";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase } from "@mui/material";
import { KeyRound, PackagePlus, TriangleAlert } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { ds, NOTICE_TONES, RADIUS } from "@/consts/designTokens";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";

/** What the forum reports about proposing (pdf-ui lib/proposeState.js). */
export type ForumProposeState = {
  status: "disconnected" | "unverified" | "noUsername" | "ready";
  verify: () => void;
  createUsername: () => void;
  propose: () => void;
};

const HeaderNotice = ({
  tone,
  message,
  actionLabel,
  onAction,
  actionTestId,
}: {
  tone: keyof typeof NOTICE_TONES;
  message: string;
  actionLabel: string;
  onAction: () => void;
  actionTestId: string;
}) => {
  const colors = NOTICE_TONES[tone];
  return (
    <Box
      data-testid="propose-notice"
      role="status"
      sx={{
        alignItems: { xxs: "flex-start", sm: "center" },
        backgroundColor: colors.bg,
        border: `1px solid ${colors.border}`,
        borderLeft: `3px solid ${colors.icon}`,
        borderRadius: `${RADIUS.md}px`,
        display: "flex",
        flexDirection: { xxs: "column", sm: "row" },
        gap: 1,
        justifyContent: "space-between",
        px: 1.75,
        py: 1,
      }}
    >
      <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 13, gap: 1, lineHeight: "18px" }}>
        <TriangleAlert aria-hidden color={colors.icon} size={15} style={{ flexShrink: 0 }} />
        {message}
      </Box>
      <ButtonBase
        data-testid={actionTestId}
        onClick={onAction}
        sx={{
          alignItems: "center",
          backgroundColor: colors.linkBg,
          borderRadius: `${RADIUS.sm}px`,
          color: colors.link,
          flexShrink: 0,
          fontFamily: "inherit",
          fontSize: 13,
          fontWeight: 600,
          gap: 0.5,
          px: 1,
          py: 0.5,
          textDecoration: "underline dotted",
          textUnderlineOffset: 3,
          "&.Mui-focusVisible": { outline: `2px solid ${colors.icon}`, outlineOffset: 2 },
        }}
      >
        <KeyRound aria-hidden size={13} />
        {actionLabel}
      </ButtonBase>
    </Box>
  );
};

/**
 * The Proposed tab's header controls: Propose a Governance Action and the
 * notice saying what proposing still needs. GovTool knows the wallet; the
 * forum reports its sign-in through `forum`, null until it has.
 */
export const useProposeHeaderControls = (forum: ForumProposeState | null) => {
  const { t } = useTranslation();
  const { isEnabled } = useCardano();
  const { openModal } = useModal();
  const connect = () => openModal({ type: "chooseWallet" });

  // A wallet still connecting reports "disconnected" to the forum; GovTool's
  // own wallet state decides that case.
  const status = !isEnabled
    ? "disconnected"
    : forum && forum.status !== "disconnected"
      ? forum.status
      : "pending";

  /** What either propose button does next. */
  const onPropose = () => {
    if (status === "disconnected") connect();
    else if (status === "ready") forum?.propose();
    else if (status === "unverified") forum?.verify();
    else if (status === "noUsername") forum?.createUsername();
  };

  const action = (
    <Button
      dataTestId="propose-a-governance-action-button"
      disabled={status !== "disconnected" && status !== "ready"}
      onClick={onPropose}
      size="large"
      startIcon={<PackagePlus aria-hidden size={18} />}
    >
      {t("govActions.propose.button")}
    </Button>
  );

  let notice: ReactNode = null;
  if (status === "disconnected") {
    notice = (
      <HeaderNotice
        actionLabel={t("govActions.propose.connectAction")}
        actionTestId="connect-wallet-link"
        message={t("govActions.propose.connectNotice")}
        onAction={connect}
        tone="info"
      />
    );
  } else if (status === "unverified" && forum) {
    notice = (
      <HeaderNotice
        actionLabel={t("govActions.propose.verifyAction")}
        actionTestId="verify-user-link"
        message={t("govActions.propose.verifyNotice")}
        onAction={forum.verify}
        tone="warning"
      />
    );
  } else if (status === "noUsername" && forum) {
    notice = (
      <HeaderNotice
        actionLabel={t("govActions.propose.usernameAction")}
        actionTestId="create-govtool-display-name-link"
        message={t("govActions.propose.usernameNotice")}
        onAction={forum.createUsername}
        tone="warning"
      />
    );
  }

  return { action, notice, onPropose };
};
