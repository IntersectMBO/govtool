import { BoxProps, TypographyProps as MUITypographyProps } from "@mui/material";

import {
  CheckboxProps,
  InputProps,
  TypographyProps,
  TextAreaProps,
} from "@atoms";

export type InputFieldProps = InputProps & {
  errorMessage?: string;
  errorStyles?: MUITypographyProps;
  helpfulText?: string;
  helpfulTextStyle?: MUITypographyProps;
  label?: string;
  labelStyles?: TypographyProps;
  layoutStyles?: BoxProps;
};

export type CheckboxFieldProps = CheckboxProps & {
  errorMessage?: string;
  errorStyles?: MUITypographyProps;
  label?: string;
  labelStyles?: TypographyProps;
  layoutStyles?: BoxProps;
};

export type TextAreaFieldProps = TextAreaProps & {
  errorMessage?: string;
  errorStyles?: MUITypographyProps;
  helpfulText?: string;
  helpfulTextStyle?: MUITypographyProps;
  label?: string;
  labelStyles?: TypographyProps;
  layoutStyles?: BoxProps;
};
