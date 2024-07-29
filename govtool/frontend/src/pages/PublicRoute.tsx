import { FC, ReactNode } from "react";
import { useLocation, useNavigate } from "react-router-dom";

import { checkIsWalletConnected } from "../utils";

export const PublicRoute: FC<{ children: ReactNode }> = ({ children }) => {
  const navigate = useNavigate();
  const { pathname } = useLocation();

  if (checkIsWalletConnected() && !pathname.startsWith("/connected")) {
    navigate(`/connected${pathname}`);
  }

  return children;
};
