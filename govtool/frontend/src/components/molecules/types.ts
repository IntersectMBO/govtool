import { SxProps } from "@mui/material";

export type LinkWithIconProps = {
  label: string;
  onClick: () => void;
  icon?: JSX.Element;
  sx?: SxProps;
  cutWithEllipsis?: boolean;
  dataTestId?: string;
};

export type StepProps = {
  label: string;
  stepNumber: number | string;
  component?: React.ReactNode;
  componentsLayoutStyles?: SxProps;
  layoutStyles?: SxProps;
};

export type DirectVoterActionProps = {
  dRepId: string;
  drepName: string;
  onCardClick: () => void;
  sx?: SxProps;
};

export type EmptyStateGovernanceActionsCategoryProps = {
  category?: string;
  isSearch?: boolean;
};

export type AutomatedVotingCardProps = {
  description: string;
  dataTestId?: string;
  inProgress?: boolean;
  isConnected?: boolean;
  isSelected?: boolean;
  onClickDelegate: () => void;
  onClickInfo: () => void;
  title: string;
  votingPower: string | number;
  isDelegateLoading?: boolean;
  transactionId?: string | null;
};
