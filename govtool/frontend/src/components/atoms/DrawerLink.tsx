import { Box, Typography } from "@mui/material";
import { FC, ReactElement } from "react";
import { NavLink } from "react-router";

import { theme } from "@/theme";

type LinkProps = {
  label: string;
  navTo: string;
  icon?: string | ReactElement;
  activeIcon?: string | ReactElement;
  dataTestId?: string;
  /** Overrides the router's prefix match, for a group's parent. */
  isActive?: boolean;
  onClick?: () => void;
};

export const DrawerLink: FC<LinkProps> = ({ ...props }) => {
  const {
    dataTestId,
    label,
    navTo,
    icon,
    activeIcon,
    isActive: isActiveOverride,
    onClick,
  } = props;
  const {
    palette: { highlightBlue },
  } = theme;

  return (
    <NavLink
      data-testid={dataTestId}
      to={navTo}
      onClick={() => {
        if (onClick) onClick();
      }}
      style={({ isActive: isRouteActive }) => ({
        textDecoration: "none",
        backgroundColor:
          (isActiveOverride ?? isRouteActive) ? highlightBlue : "transparent",
        padding: "8px 16px",
        display: "block",
        borderRadius: 100,
      })}
    >
      {({ isActive: isRouteActive }) => {
        const isActive = isActiveOverride ?? isRouteActive;
        return (
          <Box display="flex">
            {activeIcon &&
            icon &&
            typeof icon === "string" &&
            typeof activeIcon === "string" ? (
              <img
                alt="icon"
                src={isActive ? activeIcon : icon}
                style={{ marginRight: "12px" }}
              />
            ) : (
              <Box marginRight="12px" height="20px" width="20px">
                {isActive ? activeIcon : icon}
              </Box>
            )}
            <Typography
              fontSize={14}
              lineHeight="20px"
              sx={{
                fontWeight: isActive ? 500 : 400,
              }}
            >
              {label}
            </Typography>
          </Box>
        );
      }}
    </NavLink>
  );
};
