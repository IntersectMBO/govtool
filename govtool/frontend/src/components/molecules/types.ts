import { SxProps } from "@mui/material";

export type LinkWithIconProps = {
  label: string;
  onClick: () => void;
  icon?: JSX.Element;
  sx?: SxProps;
};

export type StepProps = {
  component: JSX.Element;
  label: string;
  layoutStyles?: SxProps;
  stepNumber: number | string;
};

export type AutomatedVotingCardProps = {
  description: string;
  onClickDelegate: () => void;
  onClickInfo: () => void;
  title: string;
  votingPower: string | number;
};
