import { useEffect } from "react";
import { useNavigate } from "react-router";

import { NotificationCenter, NotificationPreferencesPanel } from "@organisms";
import { PATHS } from "@consts";
import { checkIsWalletConnected } from "@utils";

// Wallet-only pages inside the Dashboard layout: notifications belong to a
// wallet, so without one they send the visitor home.
const useWalletRequired = () => {
  const navigate = useNavigate();
  useEffect(() => {
    if (!checkIsWalletConnected()) navigate(PATHS.home);
  }, []);
};

export const NotificationsPage = () => {
  useWalletRequired();
  return <NotificationCenter />;
};

export const NotificationPreferencesPage = () => {
  useWalletRequired();
  return <NotificationPreferencesPanel />;
};
