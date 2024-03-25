import { createTheme } from "@mui/material/styles";
import {
  cyan,
  errorRed,
  fadedPurple,
  orange,
  primaryBlue,
  progressYellow,
  successGreen,
} from "./consts";

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
    MuiInputBase: {
      styleOverrides: {
        root: {
          bgcolor: "white",
          borderColor: "#6F99FF",
          border: 1,
          borderRadius: 50,
          padding: "8px 16px",
          width: "100%",
        },
      },
    },
    MuiButton: {
      styleOverrides: {
        root: {
          borderRadius: 50,
          textTransform: "none",
        },
        outlined: (props) => ({
          borderColor: props.theme.palette.lightBlue,
        }),
      },
    },
    MuiChip: {
      variants: [
        {
          props: { color: "default", variant: "filled" },
          style: {
            backgroundColor: fadedPurple.c100,
          },
        },
        {
          props: { color: "success", variant: "filled" },
          style: {
            backgroundColor: successGreen.c200,
            color: successGreen.c700,
          },
        },
        {
          props: { color: "error", variant: "filled" },
          style: {
            backgroundColor: errorRed.c100,
            color: errorRed.c500,
          },
        },
        {
          props: { color: "warning", variant: "filled" },
          style: {
            backgroundColor: progressYellow.c200,
            color: orange.c700,
          },
        },
        {
          props: { color: "info", variant: "filled" },
          style: {
            backgroundColor: cyan.c100,
            color: cyan.c500,
          },
        },
      ],
      styleOverrides: {
        root: {
          fontSize: "0.875rem",
          fontWeight: 500,
          height: 28,
        },
        filledPrimary: {
          backgroundColor: primaryBlue.c100,
          color: primaryBlue.c500,
        },
        filledSecondary: {
          backgroundColor: orange.c100,
          color: orange.c600,
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
  },
  typography: {
    fontFamily: "Poppins, Arial",
    allVariants: {
      color: "#242232",
    },
  },
  palette: {
    accentOrange: "#F29339",
    accentYellow: "#F2D9A9",
    boxShadow1: "rgba(0, 18, 61, 0.37)",
    boxShadow2: "rgba(47, 98, 220, 0.2)",
    fadedPurple: "#716E88",
    highlightBlue: "#C2EFF299",
    inputRed: "#FAEAEB",
    lightBlue: "#D6E2FF",
    lightOrange: "#FFCBAD",
    negativeRed: "#E58282",
    neutralGray: "#8E908E",
    neutralWhite: "#FFFFFF",
    orangeDark: "#803205",
    positiveGreen: "#5CC165",
    primary: { main: "#0033AD" },
    primaryBlue: "#0033AD",
    secondary: { main: "rgb(255, 100, 10)" },
    secondaryBlue: "#6F99FF",
    specialCyan: "#1C94B2",
    specialCyanBorder: "#77BFD1",
    textBlack: "#242232",
    textGray: "#525252",
  },
});

theme.shadows[1] =
  "0px 1px 2px 0px rgba(0, 51, 173, 0.08), 0px 1px 6px 1px rgba(0, 51, 173, 0.15)";
theme.shadows[2] =
  "0px 1px 2px 0px rgba(0, 51, 173, 0.08), 0px 2px 10px 2px rgba(0, 51, 173, 0.15)";
theme.shadows[3] =
  "0px 1px 3px 0px rgba(0, 51, 173, 0.08), 0px 4px 12px 3px rgba(0, 51, 173, 0.15)";
theme.shadows[4] =
  "0px 2px 3px 0px rgba(0, 51, 173, 0.08), 0px 6px 14px 4px rgba(0, 51, 173, 0.15)";
theme.shadows[5] =
  "0px 4px 4px 0px rgba(0, 51, 173, 0.08), 0px 8px 20px 6px rgba(0, 51, 173, 0.15)";
