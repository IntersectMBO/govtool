import { Box, Typography } from "@mui/material";
import { FC } from "react";
import { NavLink } from "react-router-dom";

import { theme } from "@/theme";
import { PATHS, PDF_PATHS } from "@/consts";

type LinkProps = {
  label: string;
  navTo: string;
  icon?: string | JSX.Element;
  activeIcon?: string | JSX.Element;
  dataTestId?: string;
  onClick?: () => void;
};

const isRouteActive = (isActive: boolean, route: string) =>
  isActive ||
  (route ===
    `${PATHS.proposalPillar.replace("/*", "")}${
      PDF_PATHS.proposalDiscussion
    }` &&
    Object.values(PDF_PATHS).some((pdfPath) =>
      window.location.pathname.includes(pdfPath),
    ));

export const DrawerLink: FC<LinkProps> = ({ ...props }) => {
  const { dataTestId, label, navTo, icon, activeIcon, onClick } = props;
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
      style={({ isActive: routeIsActive }) => {
        // Workaround for the PDF routes not being handled by react-router
        const isActive = isRouteActive(routeIsActive, navTo);
        return {
          textDecoration: "none",
          backgroundColor: isActive ? highlightBlue : "transparent",
          padding: "8px 16px",
          display: "block",
          borderRadius: 100,
        };
      }}
    >
      {({ isActive: routeIsActive }) => {
        // Workaround for the PDF routes not being handled by react-router
        const isActive = isRouteActive(routeIsActive, navTo);
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
