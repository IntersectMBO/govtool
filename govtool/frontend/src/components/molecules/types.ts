import { SxProps } from "@mui/material";

export type LinkWithIconProps = {
  label: string;
  onClick: () => void;
  icon?: JSX.Element;
  sx?: SxProps;
};

export type StepProps = {
  label: string;
  stepNumber: number | string;
  component?: JSX.Element;
  componentsLayoutStyles?: SxProps;
  layoutStyles?: SxProps;
};
