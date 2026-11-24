import { ReactNode } from "react";
import { Box, ButtonBase, Skeleton } from "@mui/material";
import {
  AtSign,
  BadgeCheck,
  CircleCheck,
  FileText,
  Landmark,
  Wallet,
} from "lucide-react";

import type { SearchCategory } from "@/hooks/queries/useGlobalSearch";
import { ds, RADIUS } from "@/consts/designTokens";

export const CATEGORY_ICONS: Record<SearchCategory, typeof Landmark> = {
  actions: Landmark,
  dreps: BadgeCheck,
  proposals: FileText,
  outcomes: CircleCheck,
  addresses: Wallet,
  handles: AtSign,
};

const focusRing = {
  "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
};

/** A pill choosing a result type, or opening one from "Browse by type". */
export const CategoryChip = ({
  category,
  label,
  isActive = false,
  pressed,
  onClick,
  dataTestId,
}: {
  category: SearchCategory;
  label: string;
  isActive?: boolean;
  /** Set where the chip toggles a filter. */
  pressed?: boolean;
  onClick: () => void;
  dataTestId?: string;
}) => {
  const Icon = CATEGORY_ICONS[category];
  return (
    <ButtonBase
      aria-pressed={pressed}
      data-testid={dataTestId}
      onClick={onClick}
      sx={{
        alignItems: "center",
        backgroundColor: isActive ? ds.primary["100"] : "#FFFFFF",
        border: `1px solid ${isActive ? ds.primary["100"] : ds.gray["200"]}`,
        borderRadius: `${RADIUS.pill}px`,
        color: isActive ? ds.primary["700"] : ds.gray["600"],
        fontFamily: "inherit",
        fontSize: 13,
        fontWeight: isActive ? 600 : 500,
        gap: 0.875,
        lineHeight: "19.5px",
        px: 1.5,
        py: 1,
        whiteSpace: "nowrap",
        ...focusRing,
      }}
    >
      <Icon aria-hidden size={15} strokeWidth={1.5} />
      {label}
    </ButtonBase>
  );
};

export type ResultBadge = { label: string; tone: "primary" | "success" | "warning" | "error" | "neutral" };

const BADGE = {
  primary: { bg: ds.primary["100"], text: ds.primary["700"] },
  success: { bg: ds.success["100"], text: ds.success["700"] },
  warning: { bg: ds.warning["100"], text: ds.warning["700"] },
  error: { bg: ds.error["100"], text: ds.error["700"] },
  neutral: { bg: ds.gray["100"], text: ds.gray["700"] },
};

/**
 * One result: an option of the dialog's listbox. Focus stays in the search
 * field; the arrow keys move `isHighlighted` and Enter opens it.
 */
export const ResultRow = ({
  id,
  category,
  title,
  description,
  meta,
  badge,
  isHighlighted,
  onOpen,
  onHighlight,
  dataTestId,
}: {
  id: string;
  category: SearchCategory;
  title: ReactNode;
  description?: ReactNode;
  meta?: ReactNode;
  badge?: ResultBadge;
  isHighlighted: boolean;
  onOpen: () => void;
  onHighlight: () => void;
  dataTestId?: string;
}) => {
  const Icon = CATEGORY_ICONS[category];
  return (
    <Box
      aria-selected={isHighlighted}
      component="li"
      data-testid={dataTestId ?? "global-search-result"}
      id={id}
      onClick={onOpen}
      onMouseMove={onHighlight}
      role="option"
      sx={{
        backgroundColor: isHighlighted ? ds.primary["25"] : "#FFFFFF",
        border: `1px solid ${isHighlighted ? ds.primary["300"] : ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        cursor: "pointer",
        display: "flex",
        gap: 2,
        p: 2,
      }}
    >
      <Box
        aria-hidden
        sx={{
          alignItems: "center",
          backgroundColor: isHighlighted ? ds.primary["50"] : ds.gray["100"],
          borderRadius: `${RADIUS.lg}px`,
          color: isHighlighted ? ds.primary["600"] : ds.gray["600"],
          display: "flex",
          flexShrink: 0,
          height: 40,
          justifyContent: "center",
          width: 40,
        }}
      >
        <Icon size={19} strokeWidth={1.8} />
      </Box>
      <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 0.5, minWidth: 0 }}>
        <Box component="span" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, letterSpacing: "-0.09px", lineHeight: "20px", overflowWrap: "anywhere" }}>
          {title}
        </Box>
        {description && (
          <Box
            component="span"
            sx={{
              color: ds.gray["600"],
              display: "-webkit-box",
              fontSize: 13,
              lineHeight: "19.5px",
              overflow: "hidden",
              overflowWrap: "anywhere",
              WebkitBoxOrient: "vertical",
              WebkitLineClamp: 2,
            }}
          >
            {description}
          </Box>
        )}
        {meta && (
          <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px", overflowWrap: "anywhere" }}>
            {meta}
          </Box>
        )}
      </Box>
      {badge && (
        <Box
          component="span"
          sx={{
            alignSelf: "flex-start",
            backgroundColor: BADGE[badge.tone].bg,
            borderRadius: `${RADIUS.pill}px`,
            color: BADGE[badge.tone].text,
            flexShrink: 0,
            fontSize: 12,
            fontWeight: 600,
            lineHeight: "18px",
            px: 0.75,
            py: 0.25,
            whiteSpace: "nowrap",
          }}
        >
          {badge.label}
        </Box>
      )}
    </Box>
  );
};

export const ResultSkeleton = () => (
  <Box
    aria-hidden
    sx={{
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      gap: 2,
      p: 2,
    }}
  >
    <Skeleton height={40} sx={{ borderRadius: `${RADIUS.lg}px`, flexShrink: 0 }} variant="rectangular" width={40} />
    <Box sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 0.75 }}>
      <Skeleton height={14} width="24%" />
      <Skeleton height={12} width="40%" />
      <Skeleton height={10} width="14%" />
    </Box>
  </Box>
);

/** The no-matches and error panels: an icon, a title, a line and two buttons. */
export const StatePanel = ({
  icon,
  tone,
  title,
  body,
  actions,
  dataTestId,
}: {
  icon: ReactNode;
  tone: "primary" | "error";
  title: string;
  body: string;
  actions?: ReactNode;
  dataTestId: string;
}) => (
  <Box
    data-testid={dataTestId}
    role={tone === "error" ? "alert" : "status"}
    sx={{
      alignItems: "center",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.lg}px`,
      display: "flex",
      flexDirection: "column",
      gap: 1.5,
      px: 3,
      py: { xxs: 4, md: 6 },
      textAlign: "center",
    }}
  >
    <Box
      aria-hidden
      sx={{
        alignItems: "center",
        backgroundColor: tone === "error" ? ds.error["100"] : ds.primary["50"],
        borderRadius: "50%",
        color: tone === "error" ? ds.error["600"] : ds.primary["600"],
        display: "flex",
        height: 64,
        justifyContent: "center",
        width: 64,
      }}
    >
      {icon}
    </Box>
    <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, letterSpacing: "-0.33px", lineHeight: "30px", m: 0, overflowWrap: "anywhere" }}>
      {title}
    </Box>
    <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0, maxWidth: 420 }}>
      {body}
    </Box>
    {actions && (
      <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "center", mt: 1 }}>
        {actions}
      </Box>
    )}
  </Box>
);

/** A keyboard key, as in "Open Enter". */
export const Kbd = ({ children }: { children: ReactNode }) => (
  <Box
    component="kbd"
    sx={{
      backgroundColor: ds.gray["50"],
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.xs}px`,
      color: ds.gray["700"],
      fontFamily: "inherit",
      fontSize: 12,
      fontWeight: 600,
      lineHeight: "18px",
      px: 0.75,
    }}
  >
    {children}
  </Box>
);
