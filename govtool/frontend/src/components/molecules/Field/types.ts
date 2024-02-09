import { BoxProps } from "@mui/material";

import { InputProps, TypographyProps } from "@atoms";

export type InputFieldProps = InputProps & {
  errorMessage?: string;
  errorStyle?: TypographyProps;
  label?: string;
  labelStyle?: TypographyProps;
  layoutStyle?: BoxProps;
};
