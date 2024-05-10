"use client";
import { useState, useEffect } from "react";
import Image from "next/image";
import { AppBar, Box, Button, IconButton, Link, Toolbar } from "@mui/material";

import { openInNewTab } from "@/utils";
import { useMediaQuery } from "@/hooks";

import { NavLink } from "../NavLink";

import MoreVertIcon from "@mui/icons-material/MoreVert";
import MenuIcon from "@mui/icons-material/Menu";
import AppLogo from "../../../public/appLogo.svg";

const POSITION_TO_BLUR = 50;

export const Header = () => {
  const { desktop, tablet } = useMediaQuery();
  const [windowScroll, setWindowScroll] = useState(0);

  useEffect(() => {
    const onScroll = () => {
      setWindowScroll(window.scrollY);
    };

    window.addEventListener("scroll", onScroll, {
      passive: true,
    });

    return () => window.removeEventListener("scroll", onScroll);
  }, []);

  const onClickGuides = () =>
    openInNewTab(
      "https://docs.sanchogov.tools/about/what-is-sanchonet-govtool"
    );

  const onClickFAQs = () => openInNewTab("https://docs.sanchogov.tools/faqs");
  return (
    <>
      <AppBar
        component="nav"
        sx={[
          style.header,
          {
            backdropFilter:
              windowScroll > POSITION_TO_BLUR ? "blur(10px)" : "none",
            backgroundColor:
              windowScroll > POSITION_TO_BLUR
                ? "arcticWhite.700"
                : "arcticWhite.50",
          },
        ]}
      >
        <Box style={style.container}>
          <Image
            alt="App logo"
            src={AppLogo}
            style={tablet ? undefined : style.logo}
          />
          <Box sx={style.rightContainer}>
            <Box
              sx={[
                style.navItemsContainer,
                { display: { xxs: "none", lg: "grid" } },
              ]}
            >
              <NavLink to="/en" label="Dashboard" />
              <NavLink to="/drepDirectory" label="DRep Directory" />
              <NavLink to="/governanceActions" label="Governance Actions" />
              <Link
                data-testid="guides-link"
                onClick={onClickGuides}
                sx={style.link}
                variant="titleS"
              >
                Guides
              </Link>
              <Link
                data-testid="faqs-link"
                onClick={onClickFAQs}
                sx={style.link}
                variant="titleS"
              >
                FAQs
              </Link>
            </Box>
            <Button
              size={tablet ? "large" : "small"}
              variant="contained"
              sx={{ ml: 3, height: tablet ? undefined : "32px" }}
            >
              {tablet ? "Connect wallet" : "Connect"}
            </Button>
            {desktop ? null : tablet ? (
              <IconButton
                data-testid="open-drawer-button"
                sx={style.tabletIcon}
              >
                <MenuIcon color="primary" />
              </IconButton>
            ) : (
              <MoreVertIcon color="primary" sx={style.mobileIcon} />
            )}
          </Box>
        </Box>
      </AppBar>
      <Toolbar />
    </>
  );
};

const style = {
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
    color: "black",
    cursor: "pointer",
    textDecoration: "none",
    "&:hover": { color: "orange.500" },
  },
  logo: { width: 114, height: 23 },
  rightContainer: { alignItems: "center", display: "flex" },
  navItemsContainer: {
    // display: "grid",
    gridTemplateColumns: "repeat(5, auto)",
    gap: "34px",
  },
  mobileIcon: { marginLeft: "8px" },
  tabletIcon: {
    backgroundColor: "arcticWhite.900",
    marginLeft: "25px",
    border: "1px solid lightBlue",
  },
};
