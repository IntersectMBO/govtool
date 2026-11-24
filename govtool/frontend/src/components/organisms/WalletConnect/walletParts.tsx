import { ReactNode } from "react";
import { Box, ButtonBase } from "@mui/material";
import { useTranslation } from "react-i18next";
import { Check, CircleAlert, ShieldCheck, TriangleAlert, X, type LucideIcon } from "lucide-react";

import { ds, PILL_TONES, RADIUS, type PillTone } from "@/consts/designTokens";

// The building blocks the wallet connection states share.

export type Tone = "primary" | "warning" | "error" | "success" | "neutral";

const TILE: Record<Tone, { bg: string; fg: string }> = {
  primary: { bg: ds.primary["50"], fg: ds.primary["600"] },
  warning: { bg: ds.orange["50"], fg: ds.orange["700"] },
  error: { bg: ds.error["50"], fg: ds.error["600"] },
  success: { bg: ds.success["50"], fg: ds.success["700"] },
  neutral: { bg: ds.gray["100"], fg: ds.gray["700"] },
};

const PILL: Record<Tone, PillTone> = {
  primary: "primary",
  warning: "warning",
  error: "error",
  success: "success",
  neutral: "neutral",
};

export const StatusPill = ({ label, tone }: { label: string; tone: Tone }) => (
  <Box
    component="span"
    sx={{
      alignItems: "center",
      backgroundColor: PILL_TONES[PILL[tone]].bg,
      borderRadius: `${RADIUS.pill}px`,
      color: PILL_TONES[PILL[tone]].text,
      display: "inline-flex",
      flexShrink: 0,
      fontSize: 12,
      fontWeight: 600,
      gap: 0.625,
      px: 1,
      py: 0.25,
      whiteSpace: "nowrap",
    }}
  >
    <Box aria-hidden component="span" sx={{ backgroundColor: "currentColor", borderRadius: "50%", height: 6, width: 6 }} />
    {label}
  </Box>
);

/** The card layout of every wallet state: tile, status, title, body, content, buttons. */
export const WalletStateFrame = ({
  icon: Icon,
  tone,
  status,
  title,
  titleId,
  body,
  children,
  footer,
  onClose,
  closeTestId = "wallet-state-close",
}: {
  icon: LucideIcon;
  tone: Tone;
  status?: string;
  title: string;
  titleId: string;
  body?: ReactNode;
  children?: ReactNode;
  footer?: ReactNode;
  onClose?: () => void;
  closeTestId?: string;
}) => {
  const { t } = useTranslation();
  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2, textAlign: "left" }}>
      <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between" }}>
        <Box aria-hidden sx={{ alignItems: "center", backgroundColor: TILE[tone].bg, borderRadius: `${RADIUS.md}px`, color: TILE[tone].fg, display: "flex", height: 44, justifyContent: "center", width: 44 }}>
          <Icon size={20} />
        </Box>
        {status && <StatusPill label={status} tone={tone} />}
      </Box>
      <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ minWidth: 0 }}>
          <Box component="h2" id={titleId} sx={{ color: ds.gray["900"], fontSize: 22, fontWeight: 600, letterSpacing: "-0.4px", lineHeight: "30px", m: 0 }}>
            {title}
          </Box>
          {body && <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, mt: 1 }}>{body}</Box>}
        </Box>
        {onClose && (
          <ButtonBase
            aria-label={t("walletConnect.close")}
            data-testid={closeTestId}
            onClick={onClose}
            sx={{ backgroundColor: ds.gray["50"], borderRadius: "50%", color: ds.gray["600"], flexShrink: 0, height: 32, width: 32, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            <X aria-hidden size={16} />
          </ButtonBase>
        )}
      </Box>
      {children}
      {footer && (
        <Box sx={{ alignItems: "center", borderTop: `1px solid ${ds.gray["200"]}`, display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "flex-end", pt: 2 }}>
          {footer}
        </Box>
      )}
    </Box>
  );
};

/** A wallet: its icon or initial, name, a line under it, and an optional tag. */
export const WalletRow = ({
  name,
  icon,
  detail,
  tag,
  tagTone = "success",
  selected = false,
  trailing,
}: {
  name: string;
  icon?: string;
  detail?: ReactNode;
  tag?: string;
  tagTone?: "success" | "error" | "neutral";
  selected?: boolean;
  trailing?: ReactNode;
}) => (
  <Box
    sx={{
      alignItems: "center",
      backgroundColor: selected ? ds.primary["25"] : ds.gray["25"],
      border: `${selected ? 2 : 1}px solid ${selected ? ds.primary["600"] : ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      gap: 1.5,
      px: 1.75,
      py: 1.25,
    }}
  >
    {icon ? (
      <Box alt="" component="img" src={icon} sx={{ borderRadius: `${RADIUS.sm}px`, flexShrink: 0, height: 36, objectFit: "contain", width: 36 }} />
    ) : (
      <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.primary["600"], borderRadius: `${RADIUS.sm}px`, color: "#FFFFFF", display: "flex", flexShrink: 0, fontWeight: 600, height: 36, justifyContent: "center", width: 36 }}>
        {name.charAt(0).toUpperCase()}
      </Box>
    )}
    <Box sx={{ flex: 1, minWidth: 0 }}>
      <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600 }}>{name}</Box>
      {detail && <Box sx={{ color: ds.gray["600"], fontSize: 13, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}>{detail}</Box>}
    </Box>
    {tag && (
      <Box component="span" sx={{ color: tagTone === "error" ? ds.error["700"] : tagTone === "neutral" ? ds.gray["600"] : ds.success["700"], flexShrink: 0, fontSize: 12, fontWeight: 500 }}>
        {tag}
      </Box>
    )}
    {trailing}
  </Box>
);

const CALLOUT: Record<"info" | "warning" | "error" | "success", { bg: string; border: string; title: string; icon: LucideIcon }> = {
  info: { bg: ds.primary["25"], border: ds.primary["600"], title: ds.primary["700"], icon: ShieldCheck },
  warning: { bg: ds.warning["50"], border: ds.orange["600"], title: ds.orange["750"], icon: TriangleAlert },
  error: { bg: ds.error["50"], border: ds.error["600"], title: ds.error["700"], icon: CircleAlert },
  success: { bg: ds.success["50"], border: ds.success["600"], title: ds.success["700"], icon: Check },
};

/** A tinted note with a coloured left edge. */
export const Callout = ({
  tone,
  title,
  body,
  testId,
}: {
  tone: keyof typeof CALLOUT;
  title: string;
  body?: ReactNode;
  testId?: string;
}) => {
  const { bg, border, title: titleColor, icon: Icon } = CALLOUT[tone];
  return (
    <Box data-testid={testId} sx={{ backgroundColor: bg, borderLeft: `3px solid ${border}`, borderRadius: `${RADIUS.md}px`, display: "flex", gap: 1.25, px: 2, py: 1.5 }}>
      <Icon aria-hidden color={titleColor} size={16} style={{ flexShrink: 0, marginTop: 2 }} />
      <Box sx={{ minWidth: 0 }}>
        <Box sx={{ color: titleColor, fontSize: 14, fontWeight: 600 }}>{title}</Box>
        {body && <Box sx={{ color: ds.gray["700"], fontSize: 13, lineHeight: "19px", mt: 0.25, overflowWrap: "anywhere" }}>{body}</Box>}
      </Box>
    </Box>
  );
};

const buttonBase = {
  borderRadius: `${RADIUS.pill}px`,
  fontSize: 14,
  fontWeight: 600,
  gap: 0.75,
  height: 40,
  px: 2.25,
  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
  "&.Mui-disabled": { opacity: 0.6 },
};

export const PrimaryAction = ({
  children,
  onClick,
  testId,
  disabled,
  danger,
}: {
  children: ReactNode;
  onClick?: () => void;
  testId?: string;
  disabled?: boolean;
  danger?: boolean;
}) => (
  <ButtonBase
    data-testid={testId}
    disabled={disabled}
    onClick={onClick}
    sx={{
      ...buttonBase,
      backgroundColor: danger ? ds.error["700"] : ds.primary["600"],
      color: "#FFFFFF",
      "&:hover": { backgroundColor: danger ? ds.error["800"] : ds.primary["700"] },
    }}
  >
    {children}
  </ButtonBase>
);

export const SecondaryAction = ({
  children,
  onClick,
  testId,
}: {
  children: ReactNode;
  onClick?: () => void;
  testId?: string;
}) => (
  <ButtonBase
    data-testid={testId}
    onClick={onClick}
    sx={{ ...buttonBase, border: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"], "&:hover": { backgroundColor: ds.gray["50"] } }}
  >
    {children}
  </ButtonBase>
);
