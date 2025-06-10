import { forwardRef, MouseEvent } from "react";
import { NavLink } from "react-router-dom";
import { Typography } from "@mui/material";
import { useCardano } from "@context";

type LinkProps = {
  dataTestId?: string;
  isConnectWallet?: boolean;
  label: React.ReactNode;
  navTo: string;
  onClick?: (event: MouseEvent<HTMLAnchorElement>) => void;
  size?: "small" | "big";
};

export const Link = forwardRef<HTMLAnchorElement, LinkProps>(
  ({ ...props }, ref) => {
    const {
      dataTestId,
      isConnectWallet,
      label,
      navTo,
      size = "small",
      onClick,
    } = props;
    const { disconnectWallet } = useCardano();

    const fontSize = {
      small: 14,
      big: 22,
    }[size];

    return (
      <NavLink
        data-testid={dataTestId}
        to={navTo}
        style={{
          textDecoration: "none",
        }}
        onClick={(e) => {
          if (!isConnectWallet) disconnectWallet();
          if (onClick) onClick(e);
        }}
        ref={ref}
      >
        {({ isActive }) => (
          <Typography
            sx={{
              fontSize,
              fontWeight: isActive && navTo !== "" ? 600 : 500,
              color: isActive && navTo !== "" ? "#FF640A" : "textBlack",
            }}
          >
            {label}
          </Typography>
        )}
      </NavLink>
    );
  },
);
