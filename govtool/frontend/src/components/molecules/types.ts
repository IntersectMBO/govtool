import { SxProps } from "@mui/material";

export type LinkWithIconProps = {
  label: string;
  onClick: () => void;
  icon?: JSX.Element;
  sx?: SxProps;
  cutWithEllipsis?: boolean;
};

export type StepProps = {
  label: string;
  stepNumber: number | string;
  component?: React.ReactNode;
  componentsLayoutStyles?: SxProps;
  layoutStyles?: SxProps;
};

export type SoleVoterActionProps = {
  dRepId: string;
  onClickArrow: () => void;
  sx?: SxProps;
};
