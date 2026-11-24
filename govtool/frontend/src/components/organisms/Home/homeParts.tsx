import { ReactNode } from "react";
import { Box, SxProps, Theme } from "@mui/material";

import { ds, FONT_FAMILY } from "@/consts/designTokens";

/** Home's content column: 1128px, centred, with side gutters. */
export const HomeColumn = ({
  children,
  sx,
}: {
  children: ReactNode;
  sx?: SxProps<Theme>;
}) => (
  <Box
    sx={{
      boxSizing: "border-box",
      maxWidth: 1128,
      mx: "auto",
      px: { xxs: 2, md: 3, xl: 0 },
      width: "100%",
      ...sx,
    }}
  >
    {children}
  </Box>
);

/** The small uppercase label above each Home heading. */
export const Eyebrow = ({
  children,
  color = ds.gray["500"],
}: {
  children: ReactNode;
  color?: string;
}) => (
  <Box
    component="p"
    sx={{
      color,
      fontSize: 10,
      fontWeight: 700,
      letterSpacing: "1.4px",
      lineHeight: "13px",
      m: 0,
      textTransform: "uppercase",
    }}
  >
    {children}
  </Box>
);

const DISPLAY_SIZES = {
  hero: { fontSize: { xxs: 44, md: 70 }, lineHeight: { xxs: "50px", md: "80px" }, letterSpacing: { xxs: "-2px", md: "-3.5px" } },
  section: { fontSize: { xxs: 32, md: 42 }, lineHeight: { xxs: "38px", md: "49px" }, letterSpacing: "-1.7px" },
  sub: { fontSize: { xxs: 26, md: 32 }, lineHeight: { xxs: "32px", md: "44px" }, letterSpacing: "-1px" },
  card: { fontSize: 26, lineHeight: "32px", letterSpacing: "-0.65px" },
};

/** Home's Georgia display headings, in the four sizes the design uses. */
export const DisplayHeading = ({
  children,
  size = "section",
  component = "h2",
  sx,
}: {
  children: ReactNode;
  size?: keyof typeof DISPLAY_SIZES;
  component?: "h1" | "h2" | "h3";
  sx?: SxProps<Theme>;
}) => (
  <Box
    component={component}
    sx={{
      color: ds.gray["900"],
      fontFamily: FONT_FAMILY.serif,
      fontWeight: 400,
      m: 0,
      whiteSpace: "pre-line",
      ...DISPLAY_SIZES[size],
      ...sx,
    }}
  >
    {children}
  </Box>
);

/** Body copy in gray at 14 or 16px. */
export const Lead = ({
  children,
  size = 14,
  sx,
}: {
  children: ReactNode;
  size?: 14 | 16;
  sx?: SxProps<Theme>;
}) => (
  <Box
    component="p"
    sx={{
      color: ds.gray["500"],
      fontSize: size,
      letterSpacing: size === 16 ? "-0.18px" : "-0.09px",
      lineHeight: size === 16 ? "24px" : "20px",
      m: 0,
      whiteSpace: "pre-line",
      ...sx,
    }}
  >
    {children}
  </Box>
);

/** A row of small outlined circles that separates the hero from the list. */
export const CircleDivider = () => (
  <Box
    aria-hidden
    sx={{
      backgroundImage: `radial-gradient(circle at 6.5px 6px, transparent 4.6px, ${ds.gray["500"]} 5px, transparent 5.4px)`,
      backgroundRepeat: "repeat-x",
      backgroundSize: "13px 12px",
      height: 12,
      opacity: 0.6,
    }}
  />
);
