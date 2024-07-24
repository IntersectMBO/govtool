import { FC, ReactNode } from "react";
import { useLocation, useNavigate } from "react-router-dom";

import { checkIsWalletConnected } from "../utils";

export const PublicRoute: FC<{ children: ReactNode }> = ({ children }) => {
  // checkIsWalletConnected returns true if wallet is NOT connected
  const isConnected = !checkIsWalletConnected();
  const navigate = useNavigate();
  const { pathname } = useLocation();

  if (isConnected && !pathname.startsWith("/connected")) {
    navigate(`/connected${pathname}`);
  }

  return children;
};
