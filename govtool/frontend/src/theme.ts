import { createTheme } from "@mui/material/styles";
import {
  ds,
  FONT_FAMILY,
  PILL_TONES,
  RADIUS,
  SHADOWS,
} from "./consts";

export type Theme = typeof theme;

export const theme = createTheme({
  breakpoints: {
    values: {
      xxs: 0,
      xs: 375,
      sm: 425,
      md: 768,
      lg: 1024,
      xl: 1440,
    },
  },
  components: {
    MuiCssBaseline: {
      styleOverrides: {
        ":root": {
          fontFamily: FONT_FAMILY.sans,
        },
      },
    },
    MuiAccordion: {
      styleOverrides: {
        root: {
          borderRadius: `12px !important`,
        },
      },
    },
    MuiInputBase: {
      styleOverrides: {
        // Only radius and spacing here: OutlinedInput and the forum's
        // TextFields share this root and draw their own border. The full
        // redesign field look lives on the Input atom.
        root: {
          borderRadius: RADIUS.md,
          padding: "8px 10px",
          width: "100%",
        },
      },
    },
    MuiButton: {
      styleOverrides: {
        root: {
          borderRadius: RADIUS.pill,
          boxShadow: "none",
          fontWeight: 600,
          letterSpacing: "-0.09px",
          lineHeight: "20px",
          textTransform: "none",
          "& .MuiButton-startIcon": { marginLeft: 0, marginRight: 6 },
          "& .MuiButton-endIcon": { marginLeft: 6, marginRight: 0 },
          "&.Mui-disabled": {
            backgroundColor: ds.gray["200"],
            boxShadow: "none",
            color: ds.gray["400"],
          },
        },
        containedPrimary: {
          backgroundColor: ds.primary["600"],
          boxShadow: SHADOWS.innerPrimary,
          color: "#FFFFFF",
          "&:hover": {
            backgroundColor: ds.primary["700"],
            boxShadow: SHADOWS.innerPrimary,
          },
        },
        containedSecondary: {
          // Orange 600 gives white text 3.2:1; 700 reaches 4.5:1.
          backgroundColor: ds.orange["700"],
          boxShadow: SHADOWS.innerAccent,
          color: "#FFFFFF",
          "&:hover": {
            backgroundColor: ds.orange["750"],
            boxShadow: SHADOWS.innerAccent,
          },
        },
        outlined: {
          backgroundColor: "#FFFFFF",
          border: `1px solid ${ds.gray["200"]}`,
          color: ds.gray["700"],
          "&:hover": {
            backgroundColor: ds.gray["50"],
            border: `1px solid ${ds.gray["200"]}`,
          },
        },
        text: {
          color: ds.primary["600"],
          "&:hover": { backgroundColor: ds.primary["50"] },
          "&.Mui-disabled": { backgroundColor: "transparent" },
        },
      },
      variants: [
        {
          props: { variant: "soft" },
          style: {
            backgroundColor: ds.primary["100"],
            border: `1px solid ${ds.primary["200"]}`,
            color: ds.primary["700"],
            "&:hover": { backgroundColor: ds.primary["200"] },
          },
        },
      ],
    },
    MuiTooltip: {
      styleOverrides: {
        arrow: { color: ds.gray["900"] },
        tooltip: {
          backgroundColor: ds.gray["900"],
          borderRadius: RADIUS.md,
          fontSize: 12,
          lineHeight: "18px",
          padding: "8px 10px",
        },
      },
    },
    MuiChip: {
      variants: [
        {
          props: { color: "default", variant: "filled" },
          style: {
            backgroundColor: PILL_TONES.neutral.bg,
            color: PILL_TONES.neutral.text,
          },
        },
        {
          props: { color: "success", variant: "filled" },
          style: {
            backgroundColor: PILL_TONES.success.bg,
            color: PILL_TONES.success.text,
          },
        },
        {
          props: { color: "error", variant: "filled" },
          style: {
            backgroundColor: PILL_TONES.error.bg,
            color: PILL_TONES.error.text,
          },
        },
        {
          props: { color: "warning", variant: "filled" },
          style: {
            backgroundColor: PILL_TONES.warning.bg,
            color: PILL_TONES.warning.text,
          },
        },
        {
          props: { color: "info", variant: "filled" },
          style: {
            backgroundColor: PILL_TONES.primary.bg,
            color: PILL_TONES.primary.text,
          },
        },
      ],
      styleOverrides: {
        root: {
          borderRadius: RADIUS.sm,
          fontSize: "0.75rem",
          fontWeight: 600,
          height: 24,
        },
        filledPrimary: {
          backgroundColor: PILL_TONES.primary.bg,
          color: PILL_TONES.primary.text,
        },
        filledSecondary: {
          backgroundColor: ds.orange["50"],
          color: ds.orange["700"],
        },
      },
    },
    MuiPaper: {
      styleOverrides: {
        root: {
          borderRadius: 12,
        },
      },
    },
    MuiPopover: {
      defaultProps: {
        elevation: 2,
      },
    },
  },
  // shape.borderRadius stays at MUI's default 4: MUI multiplies numeric sx
  // radii by it, and screens not yet redesigned use those numbers.
  typography: {
    fontFamily: FONT_FAMILY.sans,
    allVariants: {
      color: ds.gray["900"],
    },
  },
  palette: {
    accentOrange: ds.orange["600"],
    accentYellow: "#F2D9A9",
    arcticWhite: ds.gray["25"],
    boxShadow1: "rgba(0, 18, 61, 0.37)",
    boxShadow2: "rgba(47, 98, 220, 0.2)",
    darkPurple: ds.gray["900"],
    ds,
    errorRed: ds.error["700"],
    fadedPurple: ds.gray["500"],
    highlightBlue: "#C2EFF299",
    inputRed: ds.error["50"],
    lightBlue: ds.primary["100"],
    lightOrange: ds.orange["200"],
    negativeRed: ds.error["400"],
    neutralGray: ds.gray["400"],
    neutralWhite: "#FFFFFF",
    orangeDark: ds.orange["700"],
    positiveGreen: ds.success["500"],
    primary: { main: ds.primary["600"] },
    primaryBlue: ds.primary["600"],
    secondary: { main: ds.orange["600"] },
    secondaryBlue: ds.primary["400"],
    specialCyan: "#1C94B2",
    specialCyanBorder: "#77BFD1",
    textBlack: ds.gray["900"],
    textGray: ds.gray["600"],
  },
});

theme.shadows[1] = SHADOWS.header;
theme.shadows[2] = SHADOWS.card;
theme.shadows[3] = SHADOWS.cardBlue;
theme.shadows[4] = SHADOWS.button;
theme.shadows[5] = SHADOWS.drop;
theme.shadows[6] = SHADOWS.checkbox;
