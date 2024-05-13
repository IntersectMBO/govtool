import {
  Box,
  Button,
  IconButton,
  Link,
  Drawer as MUIDrawer,
} from "@mui/material";
import Image from "next/image";
import * as NextLink from "next/link";

import { openInNewTab } from "@/utils";
import { NavLink } from "./NavLink";

import CloseIcon from "@mui/icons-material/Close";
import HelpOutlineIcon from "@mui/icons-material/HelpOutline";
import AppLogo from "../../public/appLogo.svg";
import AppLogoWithoutText from "../../public/appLogoWithoutText.svg";

export const Drawer = ({
  onClose,
  onOpen,
  isOpen,
  votingPower,
  hasDrawerBorder,
  onClickConnect,
  links,
  navigation,
}) => {
  const onClickHelp = () =>
    openInNewTab("https://docs.sanchogov.tools/support/get-help-in-discord");

  return (
    <MUIDrawer anchor="right" onClose={onClose} onOpen={onOpen} open={isOpen}>
      <Box
        sx={[
          style.topContainer,
          {
            borderBottom: hasDrawerBorder ? "1px solid lightBlue" : undefined,
          },
        ]}
      >
        <NextLink href="/">
          <Image
            alt="App logo"
            src={votingPower ? AppLogoWithoutText : AppLogo}
            style={votingPower ? style.appLogoWithoutText : style.appLogo}
          />
        </NextLink>
        <IconButton onClick={onClose}>
          <CloseIcon color="primary" />
        </IconButton>
      </Box>
      <Box sx={style.container}>
        {onClickConnect && (
          <Button variant="contained" size="large" sx={style.connectButton}>
            Connect your wallet
          </Button>
        )}
        <Box
          sx={[
            style.navigationContainer,
            {
              gridTemplateRows: `repeat(${
                (navigation?.length ?? 0) + (links?.length ?? 0)
              }, max-content)`,
            },
          ]}
        >
          {navigation?.map((navItem) => (
            <NavLink
              to={navItem.to}
              label={navItem.label}
              activeTypographyVariant="titleMediumL"
              typographyVariant="titleMediumL"
            />
          ))}
          {links?.map((link) => (
            <Link
              data-testid={`${link.label}-link`}
              onClick={link.onClick}
              sx={style.link}
              variant="titleMediumL"
            >
              {link.label}
            </Link>
          ))}
        </Box>
        <Box sx={style.spacer} />
        <Button
          data-testid="help-footer-button"
          onClick={onClickHelp}
          size="large"
          startIcon={<HelpOutlineIcon color="orange" />}
          sx={style.helpButton}
          variant="link"
        >
          Help
        </Button>
      </Box>
    </MUIDrawer>
  );
};

const style = {
  appLogo: { height: 23, width: 114 },
  appLogoWithoutText: { height: 24, width: 27 },
  navigationContainer: {
    marginTop: "48px",
    display: "grid",
    gap: "32px",
    flex: 1,
  },
  connectButton: { marginTop: "8px", width: "100%" },
  container: {
    display: "flex",
    flex: 1,
    flexDirection: "column",
    paddingX: "16px",
    paddingBottom: "24px",
  },
  topContainer: {
    paddingX: "16px",
    paddingTop: "10px",
    paddingBottom: "22px",
    alignItems: "center",
    display: "flex",
    justifyContent: "space-between",
    width: "calc(100vw - 32px)",
  },
  link: {
    color: "fadedPurple.800",
    cursor: "pointer",
    textDecoration: "none",
    "&:hover": { color: "orange.500" },
  },
  helpButton: { color: "fadedPurple.800", fontSize: 16, width: "fit-content" },
  spacer: { display: "flex", flex: 1 },
};
