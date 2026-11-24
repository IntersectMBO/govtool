// The redesign's type scale (Figma "Typography" text styles, Geist).
// The new names follow Figma; the old names map to the nearest new size and
// keep their old weights, so existing screens do not suddenly turn bold.

export type TypographyVariant =
  | "headline1"
  | "headline2"
  | "headline3"
  | "headline4"
  | "headline5"
  | "title1"
  | "title2"
  | "body1"
  | "body2"
  | "caption"
  | "heading1"
  | "heading3"
  | "heading4"
  | "heading5"
  | "heading6"
  | "paragraphXL"
  | "paragraphLG"
  | "paragraphMD"
  | "paragraphNormal"
  | "paragraphBase"
  | "paragraphSM"
  | "paragraphXS"
  | "tagline";

export type TypographyStyle = {
  fontSize: number;
  lineHeight: string;
  letterSpacing: string;
  fontWeight: number;
};

const style = (
  fontSize: number,
  lineHeight: number,
  letterSpacing: number,
  fontWeight: number,
): TypographyStyle => ({
  fontSize,
  lineHeight: `${lineHeight}px`,
  letterSpacing: `${letterSpacing}px`,
  fontWeight,
});

export const TYPOGRAPHY_VARIANTS: Record<TypographyVariant, TypographyStyle> = {
  heading1: style(72, 90, -1.61, 600),
  headline1: style(72, 90, -1.61, 600),
  heading3: style(56, 70, -1.25, 600),
  headline2: style(56, 70, -1.25, 700),
  heading4: style(48, 60, -1.07, 500),
  heading5: style(40, 50, -0.89, 600),
  heading6: style(36, 45, -0.79, 600),
  headline3: style(36, 45, -0.79, 400),
  paragraphXL: style(28, 42, -0.52, 600),
  headline4: style(28, 42, -0.52, 600),
  paragraphLG: style(24, 36, -0.47, 600),
  headline5: style(24, 36, -0.47, 500),
  paragraphMD: style(20, 30, -0.33, 600),
  title1: style(20, 30, -0.33, 400),
  paragraphNormal: style(18, 27, -0.26, 600),
  title2: style(18, 27, -0.26, 500),
  paragraphBase: style(16, 24, -0.18, 400),
  body1: style(16, 24, -0.18, 600),
  paragraphSM: style(14, 20, -0.09, 400),
  body2: style(14, 20, -0.09, 500),
  paragraphXS: style(13, 19.5, -0.04, 400),
  tagline: style(12, 18, 0.01, 400),
  caption: style(12, 18, 0.01, 400),
};
