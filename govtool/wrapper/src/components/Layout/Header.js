"use client";
import { useState, useEffect } from "react";
import Image from "next/image";
import * as NextLink from "next/link";
import {
  AppBar,
  Box,
  Button,
  IconButton,
  Link,
  Toolbar,
  Typography,
} from "@mui/material";

import { useMediaQuery } from "@/hooks";

import { Drawer } from "../Drawer";
import { NavLink } from "../NavLink";
import { VotingPowerChips } from "../VotingPowerChips";

import MoreVertIcon from "@mui/icons-material/MoreVert";
import MenuIcon from "@mui/icons-material/Menu";
import AppLogo from "../../../public/appLogo.svg";
import ArrowBackIosNewIcon from "@mui/icons-material/ArrowBackIosNew";

export const Header = ({
  onClickBackButton,
  title,
  votingPower,
  onClickConnect,
  navToOnLogo,
  votingPowerTooltipHeading,
  votingPowerTooltipParagraphOne,
  votingPowerTooltipParagraphTwo,
  headerLinks,
  headerNavigation,
  hasDrawerBorder,
  drawerNavigation,
  drawerLinks,
  positionToBlur = 50,
}) => {
  const { desktop, tablet } = useMediaQuery();
  const [windowScroll, setWindowScroll] = useState(0);
  const [isDrawerOpen, setIsDrawerOpen] = useState(false);

  const closeDrawer = () => setIsDrawerOpen(false);

  const openDrawer = () => setIsDrawerOpen(true);

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
      <AppBar
        component="nav"
        sx={[
          style.header,
          {
            backdropFilter:
              windowScroll > positionToBlur ? "blur(10px)" : "none",
            backgroundColor:
              windowScroll > positionToBlur
                ? "arcticWhite.700"
                : "arcticWhite.50",
          },
        ]}
      >
        <Box style={style.container}>
          <Box sx={style.centeredBox}>
            {onClickBackButton && (
              <IconButton onClick={onClickBackButton} sx={style.backButton}>
                <ArrowBackIosNewIcon color="primary" />
              </IconButton>
            )}
            <NextLink href={navToOnLogo ?? "/"} style={style.centeredBox}>
              <Image
                alt="App logo"
                src={AppLogo}
                style={tablet ? style.logo : style.mobileLogo}
              />
            </NextLink>
            {title && (
              <Typography sx={{ marginLeft: "25px" }} variant="headlineM">
                {title}
              </Typography>
            )}
          </Box>
          <Box sx={style.rightContainer}>
            {(headerLinks?.length || headerNavigation?.length) && (
              <Box
                sx={[
                  style.navItemsContainer,
                  {
                    display: { xxs: "none", lg: "grid" },
                    gridTemplateColumns: `repeat(${
                      (headerLinks?.length ?? 0) +
                      (headerNavigation?.length ?? 0)
                    }, auto)`,
                  },
                ]}
              >
                {headerNavigation?.map((navItem) => (
                  <NavLink to={navItem.to} label={navItem.label} />
                ))}
                {headerLinks?.map((link) => (
                  <Link
                    data-testid={`${link.label}-link`}
                    onClick={link.onClick}
                    sx={style.link}
                    variant="titleS"
                  >
                    {link.label}
                  </Link>
                ))}
              </Box>
            )}
            {onClickConnect && (
              <Button
                size={tablet ? "large" : "small"}
                onClick={onClickConnect}
                variant="contained"
                sx={{
                  ml: "24px",
                  height: tablet ? undefined : "32px",
                  whiteSpace: "nowrap",
                }}
              >
                {tablet ? "Connect wallet" : "Connect"}
              </Button>
            )}
            {votingPower && (
              <VotingPowerChips
                votingPower={votingPower}
                tooltipHeading={votingPowerTooltipHeading}
                tooltipParagraphOne={votingPowerTooltipParagraphOne}
                tooltipParagraphTwo={votingPowerTooltipParagraphTwo}
              />
            )}
            {desktop ? null : tablet ? (
              <IconButton
                data-testid="open-drawer-button"
                onClick={openDrawer}
                sx={style.tabletIcon}
              >
                <MenuIcon color="primary" />
              </IconButton>
            ) : (
              <IconButton
                data-testid="open-drawer-button"
                onClick={openDrawer}
                sx={style.mobileIcon}
              >
                <MoreVertIcon color="primary" />
              </IconButton>
            )}
          </Box>
        </Box>
      </AppBar>
      <Drawer
        onClose={closeDrawer}
        onOpen={openDrawer}
        isOpen={isDrawerOpen}
        onClickConnect={onClickConnect}
        navigation={drawerNavigation}
        links={drawerLinks}
        hasDrawerBorder={hasDrawerBorder}
      />
      <Toolbar />
    </>
  );
};

const style = {
  backButton: { marginRight: "17px", cursor: "pointer" },
  container: {
    alignItems: "center",
    display: "flex",
    flex: 1,
    justifyContent: "space-between",
    maxWidth: 1290,
  },
  header: {
    alignItems: "center",
    borderBottom: { xxs: "1px solid lightBlue", md: 0 },
    borderRadius: 0,
    boxShadow: 0,
    flex: 1,
    flexDirection: "row",
    justifyContent: "center",
    maxHeight: { xxs: 56, md: 96 },
    paddingBottom: { xxs: "18px", md: "24px" },
    paddingTop: { xxs: "6px", md: "24px" },
    paddingX: { xxs: "16px", md: "40px" },
  },
  link: {
    color: "fadedPurple.800",
    cursor: "pointer",
    textDecoration: "none",
    "&:hover": { color: "orange.500" },
  },
  centeredBox: { display: "flex", alignItems: "center" },
  mobileLogo: { width: 114, height: 23 },
  logo: { width: 114, height: 32 },
  rightContainer: { alignItems: "center", display: "flex" },
  navItemsContainer: {
    gap: "34px",
  },
  mobileIcon: { marginLeft: "8px" },
  tabletIcon: {
    backgroundColor: "arcticWhite.900",
    marginLeft: "25px",
    border: "1px solid lightBlue",
  },
};
