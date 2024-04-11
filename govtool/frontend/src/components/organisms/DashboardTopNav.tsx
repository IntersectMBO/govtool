import { useEffect, useState } from "react";
import { Box, IconButton } from "@mui/material";

import { VotingPowerChips, Typography } from "@atoms";
import { ICONS } from "@consts";
import { useScreenDimension } from "@hooks";
import { DashboardDrawerMobile } from "@organisms";

type DashboardTopNavProps = {
  title: string;
  isVotingPowerHidden?: boolean;
};

const POSITION_TO_BLUR = 50;

export const DashboardTopNav = ({
  title,
  isVotingPowerHidden,
}: DashboardTopNavProps) => {
  const [windowScroll, setWindowScroll] = useState<number>(0);
  const { isMobile } = useScreenDimension();
  const [isDrawerOpen, setIsDrawerOpen] = useState<boolean>(false);

  const openDrawer = () => {
    setIsDrawerOpen(true);
  };

  useEffect(() => {
    const onScroll = () => {
      setWindowScroll(window.scrollY);
    };

    window.addEventListener("scroll", onScroll, {
      passive: true,
    });

    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  return (
    <>
      <Box
        sx={{
          alignItems: "center",
          backdropFilter: "blur(10px)",
          backgroundColor:
            windowScroll > POSITION_TO_BLUR
              ? "rgba(256, 256, 256, 0.7)"
              : isMobile
              ? "#FBFBFF59"
              : "transparent",
          borderBottom: "1px solid #D6E2FF",
          display: "flex",
          justifyContent: "space-between",
          position: "sticky",
          minHeight: isMobile ? 36 : 48,
          px: isMobile ? 2 : 5,
          py: 3,
          top: 0,
          width: "fill-available",
          zIndex: 100,
        }}
      >
        <Box display="flex">
          {isMobile ? (
            <img
              alt="app-logo"
              height={24}
              src={ICONS.appLogoIcon}
              width={27}
            />
          ) : null}
          {!isMobile && title ? (
            <Typography variant="headline5">{title}</Typography>
          ) : null}
        </Box>
        <Box display="flex">
          {!isVotingPowerHidden && <VotingPowerChips />}
          {isMobile && (
            <IconButton
              data-testid="open-drawer-button"
              sx={{ padding: 0, marginLeft: 1 }}
              onClick={openDrawer}
            >
              <img alt="drawer" src={ICONS.drawerIcon} />
            </IconButton>
          )}
        </Box>
        {isMobile && (
          <DashboardDrawerMobile
            isDrawerOpen={isDrawerOpen}
            setIsDrawerOpen={setIsDrawerOpen}
          />
        )}
      </Box>
      {isMobile && title ? (
        <Box sx={{ borderBottom: "1px solid white", px: 2, py: 3.25 }}>
          <Typography variant="title1">{title}</Typography>
        </Box>
      ) : null}
    </>
  );
};
