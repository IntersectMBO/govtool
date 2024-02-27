import { SxProps } from "@mui/material";

export type BackToLinkProps = {
  label: string;
  onClick: () => void;
  sx?: SxProps;
};

export type AutomatedVotingCardProps = {
  description: string;
  onClickDelegate: () => void;
  onClickInfo: () => void;
  title: string;
  votingPower: string | number;
};
