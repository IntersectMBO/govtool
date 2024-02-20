import { ChangeEvent } from "react";
import {
  ButtonProps as MUIButtonProps,
  CheckboxProps as MUICheckboxProps,
  InputBaseProps,
  TypographyProps as MUITypographyProps,
  TextareaAutosizeProps,
} from "@mui/material";

export type ButtonProps = Omit<MUIButtonProps, "size"> & {
  size?: "small" | "medium" | "large" | "extraLarge";
};

export type LoadingButtonProps = ButtonProps & {
  isLoading?: boolean;
};

export type TypographyProps = Pick<
  MUITypographyProps,
  "color" | "lineHeight" | "sx"
> & {
  children?: React.ReactNode;
  fontSize?: number;
  fontWeight?: 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900;
  variant?:
    | "headline1"
    | "headline2"
    | "headline3"
    | "headline4"
    | "headline5"
    | "title1"
    | "title2"
    | "body1"
    | "body2"
    | "caption";
};

export type InputProps = InputBaseProps & {
  dataTestId?: string;
  errorMessage?: string;
};

export type SpacerProps = {
  x?: number;
  y?: number;
};

export type CheckboxProps = Omit<MUICheckboxProps, "onChange" | "value"> & {
  dataTestId?: string;
  errorMessage?: string;
  onChange: (newValue: ChangeEvent<Element> | boolean) => void;
  value: boolean;
};

export type FormErrorMessageProps = {
  errorMessage?: string;
  errorStyles?: MUITypographyProps;
};

export type FormHelpfulTextProps = {
  helpfulText?: string;
  helpfulTextStyle?: MUITypographyProps;
};

export type TextAreaProps = TextareaAutosizeProps & {
  errorMessage?: string;
};
