import { createTheme } from "@mui/material/styles";

declare module "@mui/material/styles" {
  interface BreakpointOverrides {
    xxs: true;
    xs: true;
    sm: true;
    md: true;
    lg: true;
    xl: true;
  }
  interface Palette {
    accentOrange: string;
    accentYellow: string;
    boxShadow1: string;
    boxShadow2: string;
    highlightBlue: string;
    inputRed: string;
    negativeRed: string;
    neutralGray: string;
    orangeDark: string;
    neutralWhite: string;
    positiveGreen: string;
    primaryBlue: string;
    secondaryBlue: string;
    specialCyan: string;
    specialCyanBorder: string;
    lightBlue: string;
    textBlack: string;
    textGray: string;
    lightOrange: string;
    fadedPurple: string;
  }
  interface PaletteOptions {
    accentOrange: string;
    accentYellow: string;
    boxShadow1: string;
    boxShadow2: string;
    highlightBlue: string;
    orangeDark: string;
    inputRed: string;
    negativeRed: string;
    neutralGray: string;
    neutralWhite: string;
    positiveGreen: string;
    primaryBlue: string;
    secondaryBlue: string;
    specialCyan: string;
    specialCyanBorder: string;
    lightBlue: string;
    textBlack: string;
    textGray: string;
    lightOrange: string;
    fadedPurple: string;
  }
}

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
        outlined: ({ theme }) => {
          return {
            borderColor: theme.palette.lightBlue,
          };
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
