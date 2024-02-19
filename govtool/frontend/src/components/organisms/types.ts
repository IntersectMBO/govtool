import { SxProps } from "@mui/material";

export type BgCardProps = {
  actionButtonLabel: string;
  children: React.ReactNode;
  onClickBackButton?: () => void;
  onClickActionButton: () => void;
  sx?: SxProps;
  title: string;
};
