import { ReactNode } from "react";
import { Box, SxProps, Theme } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { ArrowRight } from "lucide-react";

import { PageContainer } from "@/components/atoms/PageContainer";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";

/** A full-width band of the page with its content column. */
export const ActivityBand = ({
  children,
  background = "#FFFFFF",
  id,
  labelledBy,
  sx,
}: {
  children: ReactNode;
  background?: string;
  /** Makes the band a target for in-page links. */
  id?: string;
  /** The id of the band's heading, which names the section. */
  labelledBy?: string;
  sx?: SxProps<Theme>;
}) => (
  <Box
    aria-labelledby={labelledBy}
    component="section"
    id={id}
    sx={{ background, outline: "none", scrollMarginTop: 80 }}
    tabIndex={id ? -1 : undefined}
  >
    <PageContainer sx={{ py: { xxs: 5, md: 8 }, ...sx }}>{children}</PageContainer>
  </Box>
);

/**
 * A link with a trailing arrow: internal by default, a new tab with `href`,
 * or a jump to an element on this page with `anchor`.
 */
export const TextLink = ({
  children,
  to,
  href,
  anchor,
  state,
  dataTestId,
  label,
}: {
  children: ReactNode;
  to?: string;
  href?: string;
  /** The id of an element on this page, focusable with tabIndex -1. */
  anchor?: string;
  state?: unknown;
  dataTestId?: string;
  /** An accessible name when the visible text is not enough on its own. */
  label?: string;
}) => {
  const sx = {
    alignItems: "center",
    alignSelf: "flex-start",
    borderRadius: `${RADIUS.xs}px`,
    color: ds.primary["600"],
    display: "inline-flex",
    flexShrink: 0,
    fontSize: 14,
    fontWeight: 600,
    gap: 0.75,
    letterSpacing: "-0.09px",
    lineHeight: "20px",
    textDecoration: "none",
    "&:hover": { textDecoration: "underline" },
    "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
  } as const;
  const content = (
    <>
      {children}
      <ArrowRight aria-hidden size={16} strokeWidth={1.75} />
    </>
  );
  if (anchor) {
    return (
      <Box
        aria-label={label}
        component="a"
        data-testid={dataTestId}
        href={`#${anchor}`}
        onClick={(event: React.MouseEvent<HTMLAnchorElement>) => {
          const target = document.getElementById(anchor);
          if (!target) return;
          event.preventDefault();
          target.scrollIntoView?.({ behavior: "smooth", block: "start" });
          target.focus({ preventScroll: true });
        }}
        sx={sx}
      >
        {content}
      </Box>
    );
  }
  if (href) {
    return (
      <Box
        aria-label={label}
        component="a"
        data-testid={dataTestId}
        href={href}
        rel="noopener noreferrer"
        sx={sx}
        target="_blank"
      >
        {content}
      </Box>
    );
  }
  return (
    <Box
      aria-label={label}
      component={RouterLink}
      data-testid={dataTestId}
      state={state}
      sx={sx}
      to={to ?? "/"}
    >
      {content}
    </Box>
  );
};

/** The heading row of a section: eyebrow, title, description and a link. */
export const SectionHeading = ({
  id,
  eyebrow,
  title,
  description,
  link,
  size = "lg",
}: {
  id: string;
  eyebrow?: string;
  title: string;
  description?: string;
  link?: ReactNode;
  size?: "lg" | "md";
}) => (
  <Box
    sx={{
      alignItems: { md: "flex-end" },
      display: "flex",
      flexDirection: { xxs: "column", md: "row" },
      gap: 2,
      justifyContent: "space-between",
    }}
  >
    <Box sx={{ display: "flex", flexDirection: "column", gap: size === "lg" ? 1 : 0.375, maxWidth: 800, minWidth: 0 }}>
      {eyebrow && (
        <Box component="p" sx={{ color: ds.primary["600"], fontSize: 12, fontWeight: 600, letterSpacing: "0.01em", lineHeight: "18px", m: 0 }}>
          {eyebrow}
        </Box>
      )}
      <Box
        component="h2"
        id={id}
        sx={{
          color: ds.gray["900"],
          fontSize: size === "lg" ? { xxs: 24, md: 28 } : 20,
          fontWeight: 600,
          letterSpacing: size === "lg" ? "-0.5px" : "-0.33px",
          lineHeight: size === "lg" ? { xxs: "30px", md: "35px" } : "30px",
          m: 0,
        }}
      >
        {title}
      </Box>
      {description && (
        <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, letterSpacing: "-0.09px", lineHeight: "20px", m: 0, maxWidth: 640 }}>
          {description}
        </Box>
      )}
    </Box>
    {link}
  </Box>
);

const BADGE_TONES = {
  primary: { bg: ds.primary["100"], text: ds.primary["700"] },
  success: { bg: ds.success["100"], text: ds.success["700"] },
  warning: { bg: ds.warning["100"], text: ds.warning["700"] },
  error: { bg: ds.error["100"], text: ds.error["700"] },
  neutral: { bg: ds.gray["100"], text: ds.gray["700"] },
};

export type BadgeTone = keyof typeof BADGE_TONES;

/** A round status label; `md` for the feed, `sm` inside table rows. */
export const Badge = ({
  tone,
  size = "sm",
  children,
}: {
  tone: BadgeTone;
  size?: "sm" | "md";
  children: ReactNode;
}) => (
  <Box
    component="span"
    sx={{
      backgroundColor: BADGE_TONES[tone].bg,
      borderRadius: `${RADIUS.pill}px`,
      color: BADGE_TONES[tone].text,
      display: "inline-flex",
      flexShrink: 0,
      fontSize: 12,
      fontWeight: 600,
      lineHeight: "18px",
      px: size === "md" ? 1.125 : 0.75,
      py: size === "md" ? 0.5 : 0.125,
      whiteSpace: "nowrap",
    }}
  >
    {children}
  </Box>
);

/** The type label beside a badge. */
export const Category = ({ children }: { children: ReactNode }) => (
  <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, fontWeight: 500, lineHeight: "18px", minWidth: 0 }}>
    {children}
  </Box>
);

/** A 36px rounded square holding a 18px icon. */
export const IconTile = ({
  background,
  color,
  children,
}: {
  background: string;
  color: string;
  children: ReactNode;
}) => (
  <Box
    aria-hidden
    sx={{
      alignItems: "center",
      backgroundColor: background,
      borderRadius: "10px",
      color,
      display: "flex",
      flexShrink: 0,
      height: 36,
      justifyContent: "center",
      width: 36,
    }}
  >
    {children}
  </Box>
);

/** The white rounded card the feed and tables sit in. */
export const ActivityCard = ({
  children,
  sx,
  dataTestId,
}: {
  children: ReactNode;
  sx?: SxProps<Theme>;
  dataTestId?: string;
}) => (
  <Box
    data-testid={dataTestId}
    sx={{
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: "14px",
      boxShadow: SHADOWS.card,
      boxSizing: "border-box",
      minWidth: 0,
      px: { xxs: 2, md: 2.75 },
      ...sx,
    }}
  >
    {children}
  </Box>
);

/** A small label over a value, as the table rows use. */
export const LabelledValue = ({
  label,
  children,
  sx,
}: {
  label: string;
  children: ReactNode;
  sx?: SxProps<Theme>;
}) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 0.375, minWidth: 0, ...sx }}>
    <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, fontWeight: 500, lineHeight: "18px" }}>
      {label}
    </Box>
    {children}
  </Box>
);
