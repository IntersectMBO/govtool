import { Typography as MUITypography } from "@mui/material";

import { TYPOGRAPHY_VARIANTS } from "@/consts/typography";

import { TypographyProps } from "./types";

export const Typography = ({
  color,
  variant = "body1",
  ...props
}: TypographyProps) => {
  const { fontSize, fontWeight, letterSpacing, lineHeight } =
    TYPOGRAPHY_VARIANTS[variant];

  return (
    <MUITypography
      color={color}
      fontSize={fontSize}
      fontWeight={fontWeight}
      letterSpacing={letterSpacing}
      lineHeight={lineHeight}
      {...props}
    >
      {props.children}
    </MUITypography>
  );
};
