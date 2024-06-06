import { PropsWithChildren } from "react";
import { useLocation, useNavigate, useParams } from "react-router-dom";
import { Box, ButtonBase, Chip, CircularProgress } from "@mui/material";

import { Button, ExternalModalButton, StatusPill, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  useDelegateTodRep,
  useGetAdaHolderCurrentDelegationQuery,
  useGetAdaHolderVotingPowerQuery,
  useGetDRepListInfiniteQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import {
  Card,
  DataMissingInfoBox,
  EmptyStateDrepDirectory,
  DataMissingHeader,
  LinkWithIcon,
  Share,
} from "@molecules";
import {
  correctAdaFormat,
  correctDRepDirectoryFormat,
  isSameDRep,
  openInNewTab,
  testIdFromLabel,
} from "@utils";

type DRepDetailsProps = {
  isConnected?: boolean;
};

export const DRepDetails = ({ isConnected }: DRepDetailsProps) => {
  const { dRepID: myDRepId, pendingTransaction, stakeKey } = useCardano();
  const { t } = useTranslation();
  const navigate = useNavigate();
  const location = useLocation();
  const { openModal } = useModal();
  const { screenWidth } = useScreenDimension();
  const { dRepId: dRepParam } = useParams();
  const { delegate, isDelegating } = useDelegateTodRep();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(stakeKey);
  const { votingPower: myVotingPower } =
    useGetAdaHolderVotingPowerQuery(stakeKey);

  const displayBackButton = location.state?.enteredFromWithinApp || false;

  const { dRepData, isDRepListLoading } = useGetDRepListInfiniteQuery({
    searchPhrase: dRepParam,
  });
  const dRep = dRepData?.[0];

  if (isDRepListLoading)
    return (
      <Box
        sx={{
          display: "flex",
          flex: 1,
          height: "100%",
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <CircularProgress />
      </Box>
    );

  if (!dRep)
    return (
      <Box
        sx={{
          alignItems: "center",
          display: "flex",
          flex: 1,
          justifyContent: "center",
        }}
      >
        <EmptyStateDrepDirectory />
      </Box>
    );

  const {
    bio,
    dRepName,
    email,
    metadataStatus,
    references,
    status,
    url,
    view,
    votingPower,
  } = dRep;

  const isMe = isSameDRep(dRep, myDRepId);
  const isMyDrep = isSameDRep(dRep, currentDelegation?.dRepView);
  const isMyDrepInProgress = isSameDRep(
    dRep,
    pendingTransaction.delegate?.resourceId,
  );

  const navigateToEditDRep = () => {
    navigate(PATHS.editDrepMetadata, {
      state: { dRepName, email, bio, references },
    });
  };

  return (
    <>
      {displayBackButton ? (
        <LinkWithIcon
          data-testid="back-button"
          label={t("back")}
          onClick={() => navigate(-1)}
          sx={{ mb: 2 }}
        />
      ) : (
        <LinkWithIcon
          data-testid="go-to-drep-directory-button"
          label={t("dRepDirectory.goToDRepDirectory")}
          onClick={() =>
            navigate(
              isConnected ? PATHS.dashboardDRepDirectory : PATHS.dRepDirectory,
            )
          }
          sx={{ mb: 2 }}
        />
      )}
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
          borderRadius: 5,
          mt: isMe || isMyDrep ? 1 : 0,
          pb: 4.25,
          pt: 2.25,
        }}
      >
        {(isMe || isMyDrep) && (
          <Box
            sx={{
              display: "flex",
              alignItems: "center",
              justifyContent: "space-between",
              mb: "18px",
              ...(screenWidth <= 1020 && {
                flexDirection: "column",
                gap: 3,
              }),
            }}
          >
            <Chip
              color="primary"
              label={
                isMe
                  ? t("dRepDirectory.meAsDRep")
                  : t("dRepDirectory.myDRep", {
                      ada: correctDRepDirectoryFormat(myVotingPower),
                    })
              }
              sx={{
                boxShadow: (theme) => theme.shadows[2],
                color: (theme) => theme.palette.text.primary,
                px: 2,
                py: 0.5,
                ...(isMe && {
                  width: "351px",
                }),
                ...(isMyDrep &&
                  !isMe && {
                    width: "100%",
                  }),
                ...(screenWidth <= 1020 && {
                  width: "100%",
                }),
              }}
            />

            {isMe && (
              <Box
                sx={{
                  alignItems: "center",
                  display: "flex",
                  gap: 1,
                  ...(screenWidth < 500 && {
                    width: "100%",
                  }),
                }}
              >
                <Button
                  data-testid="edit-drep-data-button"
                  onClick={navigateToEditDRep}
                  variant="outlined"
                  sx={{
                    ...(screenWidth < 500 && {
                      width: "100%",
                    }),
                  }}
                >
                  {t("dRepDirectory.editBtn")}
                  <img
                    alt="sorting active"
                    src={ICONS.editIcon}
                    style={{ marginLeft: "4px" }}
                  />
                </Button>
                {screenWidth > 1020 && <Share link={window.location.href} />}
              </Box>
            )}
          </Box>
        )}
        <Box component="dl" gap={2} m={0}>
          <DataMissingHeader
            title={dRepName ?? undefined}
            isDataMissing={metadataStatus}
            shareLink={!isMe ? window.location.href : undefined}
          />
          {metadataStatus && (
            <DataMissingInfoBox
              isDataMissing={metadataStatus}
              isDrep
              sx={{ mb: 3 }}
            />
          )}
          {metadataStatus && !!url && (
            <ExternalModalButton
              label={t("govActions.seeExternalData")}
              sx={{ mb: 3 }}
              url={url}
            />
          )}
          <DRepDetailsInfoItem label={t("drepId")}>
            <DRepId>{view}</DRepId>
          </DRepDetailsInfoItem>
          <DRepDetailsInfoItem label={t("status")}>
            <StatusPill status={status} />
          </DRepDetailsInfoItem>
          <DRepDetailsInfoItem label={t("votingPower")}>
            <Typography
              data-testid="voting-power"
              sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}
            >
              {"₳ "}
              {correctAdaFormat(votingPower)}
            </Typography>
          </DRepDetailsInfoItem>
          {email && !metadataStatus && (
            <DRepDetailsInfoItem label={t("email")}>
              <MoreInfoLink label={email} navTo={email} isEmail />
            </DRepDetailsInfoItem>
          )}
          {references.length > 0 && !metadataStatus && (
            <DRepDetailsInfoItem label={t("moreInformation")}>
              <Box
                alignItems="flex-start"
                display="flex"
                flexDirection="column"
                gap={1.5}
              >
                {references.map((link) => (
                  <MoreInfoLink key={link} label={link} navTo={link} />
                ))}
              </Box>
            </DRepDetailsInfoItem>
          )}
        </Box>
        <Box
          sx={{
            mt:
              (!isMyDrep || !isConnected) && status === "Active"
                ? 5.75
                : undefined,
            width: screenWidth < 1024 ? "100%" : 286,
          }}
        >
          {isConnected && status === "Active" && !isMyDrep && (
            <Button
              data-testid="delegate-button"
              disabled={!!pendingTransaction.delegate}
              isLoading={
                isDelegating === dRep.view || isDelegating === dRep.drepId
              }
              onClick={() => delegate(dRep.view)}
              size="extraLarge"
              sx={{ width: "100%" }}
              variant="contained"
            >
              {t("delegate")}
            </Button>
          )}
          {!isConnected && status === "Active" && (
            <Button
              data-testid="connect-to-delegate-button"
              onClick={() =>
                openModal({
                  type: "chooseWallet",
                  state: {
                    pathToNavigate: PATHS.dashboardDRepDirectoryDRep.replace(
                      ":dRepId",
                      view,
                    ),
                  },
                })
              }
              size="extraLarge"
              sx={{ width: "100%" }}
              variant="outlined"
            >
              {t("connectToDelegate")}
            </Button>
          )}
        </Box>
        {bio && !metadataStatus && (
          <>
            <Typography variant="title2" sx={{ mb: 1.5 }}>
              {t("about")}
            </Typography>
            <Typography fontWeight={400} sx={{ maxWidth: 608 }} variant="body1">
              {bio}
            </Typography>
          </>
        )}
      </Card>
    </>
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
    <Box component="dt" sx={{ mb: 0.5, "&:not(:first-of-type)": { mt: 2 } }}>
      <Typography color="neutralGray" fontWeight={600} variant="body2">
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
    onClick={(e) => {
      if (!children) return;
      navigator.clipboard.writeText(children?.toString());
      e.stopPropagation();
    }}
    data-testid="copy-drep-id-button"
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
  isEmail?: boolean;
};

const MoreInfoLink = ({ label, navTo, isEmail = false }: LinkWithIconProps) => {
  const openLink = () => {
    if (isEmail) {
      window.location.assign(`mailto:${navTo}`);

      return;
    }
    openInNewTab(navTo);
  };

  return (
    <ButtonBase
      data-testid={`${testIdFromLabel(label)}-link`}
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
