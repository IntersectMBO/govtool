import { PropsWithChildren, useState } from "react";
import { Navigate, useNavigate, useParams } from "react-router-dom";
import {
  Box, ButtonBase, Chip, CircularProgress
} from "@mui/material";

import { Button, LoadingButton, Typography } from "@atoms";
import { Card, Share } from "@molecules";
import { ICONS, PATHS } from "@consts";
import {
  useGetAdaHolderCurrentDelegationQuery,
  useGetDRepListQuery,
  useScreenDimension,
  useTranslation
} from "@hooks";
import { correctAdaFormat, openInNewTab } from "@utils";
import { useCardano, useModal, useSnackbar } from "@/context";

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
  const {
    buildSignSubmitConwayCertTx,
    buildVoteDelegationCert,
    dRepID: myDRepId,
    pendingTransaction,
    stakeKey
  } = useCardano();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const { openModal } = useModal();
  const { addSuccessAlert, addErrorAlert } = useSnackbar();
  const { screenWidth } = useScreenDimension();
  const { dRepId: dRepParam } = useParams();

  const [isDelegating, setIsDelegating] = useState(false);

  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { data, isLoading } = useGetDRepListQuery(dRepParam);
  const dRep = data?.[0];

  if (!dRep && isLoading) return <CircularProgress />;

  if (!dRep) return <Navigate to={PATHS.error} />;

  const {
    drepId, view, status, votingPower, type
  } = dRep;

  const isMe = drepId === myDRepId || view === myDRepId;
  const isMyDrep = drepId === currentDelegation || view === currentDelegation;
  const inProgressDelegation = pendingTransaction.delegate?.resourceId;
  const isMyDrepInProgress = drepId === inProgressDelegation || view === inProgressDelegation;

  const delegate = async () => {
    setIsDelegating(true);
    try {
      const certBuilder = await buildVoteDelegationCert(drepId);
      const result = await buildSignSubmitConwayCertTx({
        certBuilder,
        type: "delegate",
        resourceId: drepId,
      });
      if (result) {
        addSuccessAlert(t("alerts.delegate.success"));
      }
    } catch (error) {
      addErrorAlert(t("alerts.delegate.failed"));
    } finally {
      setIsDelegating(false);
    }
  };

  return (
    <Card
      {...((isMe || isMyDrep) && {
        border: true,
        variant: "primary",
      })}
      {...(isMyDrepInProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      sx={{
        borderRadius: 5, mt: isMe || isMyDrep ? 1 : 0, pb: 4, pt: 2.25
      }}
    >
      {(isMe || isMyDrep) && (
        <Chip
          color="primary"
          label={isMe ? t("dRepDirectory.meAsDRep") : t("dRepDirectory.myDRep")}
          sx={{
            boxShadow: (theme) => theme.shadows[2],
            color: (theme) => theme.palette.text.primary,
            mb: 1.5,
            px: 2,
            py: 0.5,
            width: '100%',
          }}
        />
      )}
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flexDirection: "row",
          gap: 1,
          mb: 3
        }}
      >
        <Typography
          fontWeight={600}
          sx={{ ...ellipsisStyles, flex: 1 }}
          variant="title2"
        >
          {type}
        </Typography>
        {isMe && (
          <Button
            onClick={() => navigate(PATHS.updateMetadata)}
            variant="outlined"
          >
            {t("dRepDirectory.editBtn")}
          </Button>
        )}
        <Share link={window.location.href} />
      </Box>

      <Box component="dl" gap={2} m={0}>
        <DRepDetailsInfoItem label={t("drepId")}>
          <DRepId>{view}</DRepId>
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("status")}>
          {/* TODO: add status pill */}
          {/* <StatusPill /> */}
          <Typography>{status}</Typography>
        </DRepDetailsInfoItem>
        <DRepDetailsInfoItem label={t("votingPower")}>
          <Typography sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}>
            {'₳ '}
            {correctAdaFormat(votingPower)}
          </Typography>
        </DRepDetailsInfoItem>
        {/* TODO: fetch metadata, add views for metadata errors */}
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
          <LoadingButton
            data-testid="delegate-button"
            disabled={!!pendingTransaction.delegate}
            isLoading={isDelegating}
            onClick={delegate}
            size="extraLarge"
            sx={{ width: "100%" }}
            variant="contained"
          >
            {t("delegate")}
          </LoadingButton>
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
        {/* TODO replace with actual data */}
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
