import { forwardRef, MouseEvent } from "react";
import { NavLink } from "react-router-dom";
import { Typography } from "@mui/material";
import { useCardano } from "@context";

const FONT_SIZE = {
  small: 14,
  big: 22,
};

type LinkProps = {
  dataTestId?: string;
  isConnectWallet?: boolean;
  label: React.ReactNode;
  navTo: string;
  onClick?: (event: MouseEvent<HTMLElement>) => void;
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

    const fontSize = FONT_SIZE[size];

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

// This component is used as a placeholder for links that do not navigate anywhere,
// but with the same styling as the Link component.
export const FakeLink = forwardRef<HTMLElement, Omit<LinkProps, "navTo">>(
  (props, ref) => {
    const {
      dataTestId,
      isConnectWallet,
      label,
      size = "small",
      onClick,
    } = props;
    const { disconnectWallet } = useCardano();

    const fontSize = FONT_SIZE[size];

    return (
      <Typography
        data-testid={dataTestId}
        onClick={(e) => {
          e.preventDefault();
          if (!isConnectWallet) disconnectWallet();
          if (onClick) onClick(e);
        }}
        ref={ref}
        sx={{
          cursor: "pointer",
          fontSize,
          fontWeight: 500,
          color: "textBlack",
        }}
      >
        {label}
      </Typography>
    );
  },
);
