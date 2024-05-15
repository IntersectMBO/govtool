import { Box, Drawer, Link } from "@mui/material";
import Image from "next/image";
import * as NextLink from "next/link";

import AppLogo from "../../../public/appLogo.svg";
import { DrawerNavLink } from "../DrawerNavLink";
import { WalletCard } from "../WalletCard";
import { DrepCard } from "../DrepCard";

export const Sidebar = ({
  dRepAddress,
  onClickDisconnect,
  navigation,
  navigationLinks,
  network,
  walletAddress,
}) => {
  return (
    <Drawer variant="permanent" anchor="left" sx={style.container} open>
      {/* TODO: Add paths */}
      <NextLink href={"/dashboard"}>
        <Image alt="App logo" src={AppLogo} style={style.logo} />
      </NextLink>
      <Box
        sx={[
          style.navigationContainer,
          {
            gridTemplateRows: `repeat(${
              (navigation?.length ?? 0) + (navigationLinks?.length ?? 0)
            }, max-content)`,
          },
        ]}
      >
        {navigation?.map((navItem) => (
          <DrawerNavLink {...navItem} />
        ))}
        {navigationLinks?.map((link) => (
          <Link
            data-testid={`${link.label}-link`}
            onClick={link.onClick}
            sx={style.link}
            variant="bodyRegularM"
          >
            <Image alt="Sidebar icon" src={link.icon} style={style.image} />
            {link.label}
          </Link>
        ))}
      </Box>
      {dRepAddress && <DrepCard address={dRepAddress} />}
      {walletAddress && (
        <WalletCard
          address={walletAddress}
          network={network}
          onClickDisconnect={onClickDisconnect}
        />
      )}
    </Drawer>
  );
};

const style = {
  container: {
    width: "268px",
    "& .MuiDrawer-paper": {
      backgroundColor: "arcticWhite.900",
      borderRight: "1px solid white",
      boxSizing: "border-box",
      padding: "32px 24px 16px 24px",
      width: "268px",
    },
  },
  image: { height: "20px", marginRight: "12px", width: "20px" },
  link: {
    alignItems: "center",
    color: "fadedPurple.800",
    display: "flex",
    textDecoration: "none",
    height: "36px",
    borderRadius: "100px",
    padding: "8px 16px 8px 16px",
    boxSizing: "border-box",
  },
  logo: { height: 32 },
  navigationContainer: {
    marginTop: "74px",
    display: "grid",
    flex: 1,
    flexDirection: "column",
    gap: "10px",
  },
};
