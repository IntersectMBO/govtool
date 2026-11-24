import { useEffect } from "react";
import { useQuery } from "@tanstack/react-query";
import { useLocation, useNavigate } from "react-router";

import { PATHS } from "@/consts/paths";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import { lovelaceFromCborValue } from "@/utils/profileMenu";
import { isWalletOnlyPath } from "@/utils/walletOnlyPath";

/** The connected wallet's ada, in lovelace, read from the wallet itself. */
export const useWalletBalance = () => {
  const { walletApi, stakeKey } = useCardano();
  const { data, isLoading, isError } = useQuery({
    queryKey: ["connectedWalletBalance", stakeKey],
    queryFn: async () => lovelaceFromCborValue(await walletApi!.getBalance()) ?? null,
    enabled: !!walletApi && !!stakeKey,
    staleTime: 30_000,
    // main.tsx turns this off app-wide; the popup reads it afresh on opening.
    refetchOnMount: true,
  });
  return { balance: data ?? undefined, isLoading, isError };
};

// Set by Switch wallet before its reload, read once by the next page.
const SWITCH_KEY = "govtool_switch_wallet";

const storage = {
  set: () => {
    try {
      sessionStorage.setItem(SWITCH_KEY, "1");
    } catch {
      // Without storage the chooser simply does not reopen.
    }
  },
  take: () => {
    try {
      const value = sessionStorage.getItem(SWITCH_KEY);
      sessionStorage.removeItem(SWITCH_KEY);
      return value === "1";
    } catch {
      return false;
    }
  },
};

/** Opens the wallet chooser on the page Switch wallet reloaded into. */
export const useOpenChooserAfterSwitch = () => {
  const { openModal } = useModal();
  useEffect(() => {
    if (storage.take()) openModal({ type: "chooseWallet" });
  }, []);
};

/**
 * Disconnecting, as the wallet menu has always done it: confirm, disconnect,
 * leave a wallet-only page, reload. `switchWallet` does the same without
 * asking and reopens the wallet chooser after the reload: several queries are
 * keyed without the stake key, so only a reload drops the old wallet's data.
 */
export const useDisconnectWallet = () => {
  const { disconnectWallet } = useCardano();
  const { openModal } = useModal();
  const navigate = useNavigate();
  const { pathname } = useLocation();

  // A page that works without a wallet stays; the rest need one.
  const leaveWalletOnlyPage = () => {
    if (isWalletOnlyPath(pathname)) navigate(PATHS.home);
  };

  const disconnect = async () => {
    await disconnectWallet();
    leaveWalletOnlyPage();
    window.location.reload();
  };

  return {
    /** Asks for confirmation, then disconnects. */
    confirmDisconnect: () =>
      openModal({ type: "disconnectWallet", state: { onConfirm: disconnect } }),
    switchWallet: async () => {
      await disconnectWallet();
      leaveWalletOnlyPage();
      storage.set();
      window.location.reload();
    },
  };
};
