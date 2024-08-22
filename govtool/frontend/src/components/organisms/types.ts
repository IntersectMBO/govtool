import { Dispatch, SetStateAction } from "react";

export type DashboardDrawerMobileProps = {
  isDrawerOpen: boolean;
  setIsDrawerOpen: Dispatch<SetStateAction<boolean>>;
};

export type DrawerMobileProps = DashboardDrawerMobileProps & {
  isConnectButton: boolean;
};
