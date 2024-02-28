import { Box, ButtonBase } from "@mui/material";

import { Button, Typography } from "@atoms";
import { Card, Share } from "@molecules";
import { ICONS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { openInNewTab } from "@utils";
import { PropsWithChildren } from "react";
import { useModal } from "@/context";

const LINKS = [
  "darlenelonglink1.DRepwebsiteorwhatever.com",
  "darlenelonglink2.DRepwebsiteorwhatever.com",
  "darlenelonglink3.DRepwebsiteorwhatever.com",
  "darlenelonglink4.DRepwebsiteorwhatever.com",
  "darlenelonglink5.DRepwebsiteorwhatever.com",
];

type DRepDetailsProps = {
  isConnected?: boolean;
};

export const DRepDetails = ({ isConnected }: DRepDetailsProps) => {
  const { t } = useTranslation();
  const { openModal } = useModal();
  const { screenWidth } = useScreenDimension();

  return (
    <Card sx={{ borderRadius: 5, pb: 4, pt: 2.25 }}>
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flexDirection: "row",
          justifyContent: "space-between",
          mb: 3
        }}
      >
        <Typography
          fontWeight={600}
          sx={ellipsisStyles}
          variant="title2"
        >
          ExampleDRepName
        </Typography>
        {/* TODO: connect link */}
        <Share link="test" />
      </Box>

      <Box component="dl" gap={2} m={0}>
        <DRepDetailsInfoItem label={t("drepId")}>
          <DRepId>
            drep_1njkdnkjwenfk12321ndcnsjdcsndc
          </DRepId>
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("status")}>
          {/* TODO: add status pill */}
          {/* <StatusPill /> */}
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("votingPower")}>
          <Typography sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}>
            {'₳ '}
            50,000,000
          </Typography>
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("email")}>
          <LinkWithIcon
            label="darlenelonglink.DRepwebsiteorwhatever.com"
            navTo="google.com"
          />
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("moreInformation")}>
          <Box alignItems="flex-start" display="flex" flexDirection="column" gap={1.5}>
            {LINKS.map((link) => (
              <LinkWithIcon key={link} label={link} navTo={link} />
            ))}
          </Box>
        </DRepDetailsInfoItem>
      </Box>

      <Box
        sx={{
          my: 5.75,
          width: screenWidth < 1024 ? "100%" : 286,
        }}
      >
        {isConnected ? (
          <Button
            // TODO: add delegate function
            onClick={() => { }}
            size="extraLarge"
            sx={{ width: "100%" }}
          >
            {t("delegate")}
          </Button>
        ) : (
          <Button
            onClick={() => openModal({ type: "chooseWallet" })}
            size="extraLarge"
            sx={{ width: "100%" }}
            variant="outlined"
          >
            {t("connectToDelegate")}
          </Button>
        )}
      </Box>

      <Typography variant="title2" sx={{ mb: 1.5 }}>{t("about")}</Typography>
      <Typography
        fontWeight={400}
        sx={{ maxWidth: 608 }}
        variant="body1"
      >
        I am the Cardano crusader carving his path in the blockchain
        battleground. With a mind sharper than a Ledger Nano X, this fearless
        crypto connoisseur fearlessly navigates the volatile seas of Cardano,
        turning code into currency. Armed with a keyboard and a heart pumping
        with blockchain beats, Mister Big Bad fearlessly champions
        decentralization, smart contracts, and the Cardano community. His
        Twitter feed is a mix of market analysis that rivals CNBC and memes that
        could break the internet.
      </Typography>
    </Card>
  );
};

const ellipsisStyles = {
  overflow: "hidden",
  textOverflow: "ellipsis",
  whiteSpace: "nowrap",
};

type DrepDetailsInfoItemProps = PropsWithChildren & {
  label: string;
};

const DRepDetailsInfoItem = ({ children, label }: DrepDetailsInfoItemProps) => (
  <>
    <Box component="dt" sx={{ mb: 0.5, '&:not(:first-of-type)': { mt: 2 } }}>
      <Typography
        color="neutralGray"
        fontWeight={600}
        variant="body2"
      >
        {label}
      </Typography>
    </Box>
    <Box component="dd" m={0}>
      {children}
    </Box>
  </>
);

const DRepId = ({ children }: PropsWithChildren) => (
  <ButtonBase
    sx={{
      gap: 1,
      maxWidth: "100%",
      "&:hover": {
        opacity: 0.6,
        transition: "opacity 0.3s",
      },
    }}
  >
    <Typography color="primary" fontWeight={500} sx={ellipsisStyles}>
      {children}
    </Typography>
    <img alt="" src={ICONS.copyBlueIcon} />
  </ButtonBase>
);

type LinkWithIconProps = {
  label: string;
  navTo: string;
};

const LinkWithIcon = ({ label, navTo }: LinkWithIconProps) => {
  const openLink = () => openInNewTab(navTo);

  return (
    <ButtonBase
      onClick={openLink}
      sx={{
        gap: 0.5,
        maxWidth: "100%",
        "&:hover": {
          opacity: 0.6,
          transition: "opacity 0.3s",
        },
      }}
    >
      <img alt="link" height={16} src={ICONS.link} width={16} />
      <Typography
        color="primary"
        fontWeight={400}
        sx={{
          overflow: "hidden",
          textDecoration: "underline",
          textOverflow: "ellipsis",
        }}
      >
        {label}
      </Typography>
    </ButtonBase>
  );
};
