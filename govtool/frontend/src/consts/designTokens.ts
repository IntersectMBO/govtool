// Design tokens from the "Gov.tools [REDESIGN]" Figma file (version of
// 2026-10-15). Scales are keyed by Figma step. Steps the file never names
// are left out; read them from the Figma export when a screen needs one.
// This module imports nothing: theme.ts and the consts barrel import each
// other, and an import here could close a cycle.

export type ColorScale = Record<string, string>;

const gray: ColorScale = {
  25: "#FCFCFD",
  50: "#F8F9FC",
  100: "#F1F4F9",
  200: "#E5E9F0",
  300: "#D5DBE5",
  400: "#98A2B3",
  500: "#667085",
  600: "#475467",
  700: "#344054",
  // No named style in the file; the value its frames use on 425 nodes.
  800: "#182230",
  900: "#101828",
  950: "#0B1220",
};

const primary: ColorScale = {
  25: "#F5F8FF",
  50: "#EEF3FF",
  100: "#E0E9FF",
  200: "#C6D5FF",
  300: "#9DB5FF",
  400: "#668BFF",
  500: "#315BEF",
  600: "#1447E6",
  700: "#0038C7",
  800: "#12369B",
  900: "#142F82",
};

const success: ColorScale = {
  50: "#E3F8EA",
  100: "#D1FADF",
  200: "#A6F4C5",
  300: "#6CE9A6",
  400: "#32D583",
  500: "#12B76A",
  600: "#039855",
  700: "#027A48",
};

const warning: ColorScale = {
  25: "#FFFCF5",
  50: "#FFFAEB",
  100: "#FEF0C7",
  400: "#FDB022",
  500: "#F79009",
  600: "#DC6803",
  700: "#B54708",
};

const error: ColorScale = {
  25: "#FFFBFA",
  50: "#FEF3F2",
  100: "#FEE4E2",
  200: "#FECDCA",
  300: "#FDA29B",
  400: "#F97066",
  500: "#F04438",
  600: "#D92D20",
  700: "#B42318",
  800: "#912018",
  900: "#7A271A",
};

const orange: ColorScale = {
  25: "#FFF9F5",
  50: "#FFF4ED",
  100: "#FFE8D6",
  200: "#FFD0B0",
  300: "#FFA26C",
  400: "#FF833B",
  500: "#FF640A",
  600: "#E9682F",
  700: "#C94F22",
  // Not in the file: orange 700 darkened 5%, the lightest that reaches 4.5:1
  // on the nav's orange tint (#FFF3EC). Text on the tint uses it.
  750: "#BF4B20",
  800: "#401903",
};

export const ds = { gray, primary, success, warning, error, orange };

export type DesignScales = typeof ds;

export const RADIUS = {
  xs: 4,
  sm: 6,
  md: 8,
  lg: 12,
  xl: 16,
  pill: 999,
};

export const SHADOWS = {
  // Figma "Blue-Shadown": the header.
  header: "0px 4px 8px 0px #2F62DC0A",
  // Figma "Dark-Shadow".
  card: "0px 8px 16px 0px #10182805",
  // Figma "Card Shadow Blue".
  cardBlue: "2px 2px 20px 0px #2F62DC33",
  // Figma "Button shadow".
  button: "1px 2px 11px 0px #00123D5E",
  // Figma "Drop Shadow/200".
  drop: "0px 1px 4px 0px #0C0C0D0D, 0px 1px 4px 0px #0C0C0D1A",
  // Figma "Drop Shadow/Checkbox".
  checkbox: "1px 2px 11px 0px #00123D33",
  // The highlight inside the primary and orange buttons.
  innerPrimary: "inset 0px 4px 4px 0px #5C7FFF",
  innerAccent: "inset 0px 4px 4px 0px #FF995C",
  // The red button's top highlight, error 400 (the Retire button).
  innerError: "inset 0px 4px 4px 0px #F97066",
};

// @fontsource registers Geist as "Geist Sans"; "Geist" covers a locally
// installed copy.
export const FONT_FAMILY = {
  sans: '"Geist Sans", "Geist", Arial, sans-serif',
  mono: '"Geist Mono", ui-monospace, SFMono-Regular, Menlo, monospace',
  serif: 'Georgia, "Times New Roman", serif',
};

// Status and vote pills (20px high, 6px radius, 12/18 semibold text).
export const PILL_TONES = {
  success: { bg: success["50"], border: success["100"], text: success["700"] },
  primary: { bg: primary["50"], border: primary["100"], text: primary["700"] },
  neutral: { bg: gray["100"], border: gray["100"], text: gray["700"] },
  error: { bg: error["50"], border: error["100"], text: error["700"] },
  warning: { bg: warning["50"], border: warning["100"], text: warning["700"] },
};

/**
 * The header notices (what proposing still needs): tint, border, icon, and
 * the action link on its own lighter tint. Orange text on an orange tint
 * uses 750, as on the nav's tint.
 */
export const NOTICE_TONES = {
  info: {
    bg: primary["25"],
    border: primary["200"],
    icon: primary["600"],
    link: primary["700"],
    linkBg: primary["50"],
  },
  warning: {
    bg: orange["25"],
    border: orange["200"],
    icon: orange["600"],
    link: orange["750"],
    linkBg: orange["50"],
  },
};

export type PillTone = keyof typeof PILL_TONES;
