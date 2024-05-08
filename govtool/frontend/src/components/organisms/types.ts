import { SxProps } from "@mui/material";
import { Dispatch, SetStateAction } from "react";

export type BgCardProps = {
  actionButtonLabel: string;
  actionButtonDataTestId?: string;
  backButtonLabel?: string;
  children: React.ReactNode;
  isLoadingActionButton?: boolean;
  isActionButtonDisabled?: boolean;
  onClickBackButton?: () => void;
  onClickActionButton: () => void;
  sx?: SxProps;
};

export type DashboardDrawerMobileProps = {
  isDrawerOpen: boolean;
  setIsDrawerOpen: Dispatch<SetStateAction<boolean>>;
};

export type DrawerMobileProps = DashboardDrawerMobileProps & {
  isConnectButton: boolean;
};
