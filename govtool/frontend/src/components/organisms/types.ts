import { SxProps } from "@mui/material";

export type BgCardProps = {
  actionButtonLabel: string;
  backButtonLabel?: string;
  children: React.ReactNode;
  isLoadingActionButton?: boolean;
  isActionButtonDisabled?:boolean;
  onClickBackButton?: () => void;
  onClickActionButton: () => void;
  sx?: SxProps;
};
