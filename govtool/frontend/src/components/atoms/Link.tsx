import { FC } from "react";
import { NavLink } from "react-router-dom";
import { Typography } from "@mui/material";
import { useCardano } from "@context";

type LinkProps = {
  dataTestId?: string;
  isConnectWallet?: boolean;
  label: string;
  navTo: string;
  onClick?: () => void;
  size?: "small" | "big";
};

export const Link: FC<LinkProps> = ({ ...props }) => {
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
      onClick={() => {
        if (!isConnectWallet) disconnectWallet();
        if (onClick) onClick();
      }}
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
};
