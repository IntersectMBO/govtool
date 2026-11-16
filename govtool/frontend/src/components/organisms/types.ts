import { Dispatch, SetStateAction } from "react";
import type { NavEntry } from "./topNavItems";

export type DrawerMobileProps = {
  isDrawerOpen: boolean;
  setIsDrawerOpen: Dispatch<SetStateAction<boolean>>;
  isConnectButton: boolean;
  isConnected: boolean;
  navEntries: NavEntry[];
  onConnect: () => void;
  /** In a wallet flow: called instead of following an in-app link. */
  onNavigateAway?: () => void;
};
