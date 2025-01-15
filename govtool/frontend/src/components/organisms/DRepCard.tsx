import { useNavigate } from "react-router-dom";
import { Box, ButtonBase, Divider, Avatar } from "@mui/material";

import { Button, StatusPill, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useModal, useSnackbar } from "@context";
import { useTranslation } from "@hooks";
import { DRepData, DRepStatus } from "@models";
import { Card } from "@molecules";
import {
  correctDRepDirectoryFormat,
  ellipsizeText,
  encodeCIP129Identifier,
  getBase64ImageDetails,
  getMetadataDataMissingStatusTranslation,
} from "@utils";

type DRepCardProps = {
  dRep: DRepData;
  isConnected: boolean;
  isDelegationLoading?: boolean;
  isInProgress?: boolean;
  isMe?: boolean;
  isMyDrep?: boolean;
  onDelegate?: () => void;
};

export const DRepCard = ({
  dRep: {
    status,
    type,
    view,
    votingPower,
    givenName,
    metadataStatus,
    image,
    drepId,
    isScriptBased,
  },
  isConnected,
  isDelegationLoading,
  isInProgress,
  isMe,
  isMyDrep,
  onDelegate,
}: DRepCardProps) => {
  const navigate = useNavigate();
  const { t } = useTranslation();
  const { addSuccessAlert } = useSnackbar();
  const { openModal } = useModal();

  const openChooseWalletModal = () =>
    openModal({
      type: "chooseWallet",
      state: {
        pathToNavigate: PATHS.dashboardDRepDirectoryDRep.replace(
          ":dRepId",
          view,
        ),
      },
    });

  const cip129Identifier = encodeCIP129Identifier({
    txID: `${isScriptBased ? "23" : "22"}${drepId}`,
    bech32Prefix: isScriptBased ? "drep_script" : "drep",
  });

  const base64Image = getBase64ImageDetails(image ?? "");

  return (
    <Card
      {...(isMe && {
        variant: "primary",
      })}
      {...(metadataStatus && {
        variant: "error",
      })}
      {...(isInProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      dataTestId={`${view}-${
        onDelegate
          ? "drep"
          : isInProgress
          ? "delegation-in-progress"
          : "delegated"
      }-card`}
      sx={{ container: "root / inline-size", py: 2.5 }}
    >
      {isMe && (
        <StatusPill
          status={DRepStatus.Yourself}
          sx={{ mb: 1.5, display: { lg: "none" } }}
        />
      )}
      <Box
        display="flex"
        flexDirection="column"
        rowGap={4}
        columnGap={6}
        sx={{
          "@container root (min-width: 480px)": {
            flexDirection: "row",
          },
        }}
      >
        <Box flex={2} minWidth={0} sx={{ containerType: "inline-size" }}>
          <Box
            display="flex"
            flexDirection="column"
            rowGap={3}
            columnGap={10}
            sx={{
              "@container (min-width: 480px)": {
                flexDirection: "row",
                alignItems: "center",
              },
              containerType: "inline-size",
            }}
          >
            <Box flexDirection="row" minWidth={0} display="flex">
              <Avatar
                alt="drep-image"
                src={
                  (base64Image.isValidBase64Image
                    ? `${base64Image.base64Prefix}${image}`
                    : image) || ICONS.defaultDRepIcon
                }
                data-testid="drep-image"
              />
              <Box
                sx={{
                  marginLeft: {
                    xxs: 1,
                    xs: 2,
                    sm: 3,
                  },
                }}
              >
                <Typography
                  sx={{ ellipsisStyles, color: metadataStatus && "errorRed" }}
                >
                  {metadataStatus
                    ? getMetadataDataMissingStatusTranslation(metadataStatus)
                    : ellipsizeText(givenName ?? "", 25)}
                </Typography>
                <Box
                  sx={{
                    display: "flex",
                    flexDirection: "column",
                  }}
                >
                  <ButtonBase
                    data-testid={`${view}-copy-id-button`}
                    onClick={(e) => {
                      navigator.clipboard.writeText(view);
                      addSuccessAlert(t("alerts.copiedToClipboard"));
                      e.stopPropagation();
                    }}
                    sx={{
                      gap: 1,
                      width: "250px",
                      maxWidth: {
                        xxs: "200px",
                        xs: "100%",
                      },
                      "&:hover": {
                        opacity: 0.6,
                        transition: "opacity 0.3s",
                      },
                    }}
                  >
                    <Typography
                      color="primary"
                      variant="body2"
                      sx={ellipsisStyles}
                    >
                      {view}
                    </Typography>
                    <img alt="" src={ICONS.copyBlueIcon} />
                  </ButtonBase>
                  <ButtonBase
                    data-testid={`${cip129Identifier}-copy-id-button`}
                    onClick={(e) => {
                      navigator.clipboard.writeText(cip129Identifier);
                      addSuccessAlert(t("alerts.copiedToClipboard"));
                      e.stopPropagation();
                    }}
                    sx={{
                      gap: 1,
                      width: "250px",
                      maxWidth: {
                        xxs: "200px",
                        xs: "100%",
                      },
                      "&:hover": {
                        opacity: 0.6,
                        transition: "opacity 0.3s",
                      },
                      display: "flex",
                      flexDirection: "row",
                    }}
                  >
                    <Typography variant="body2" sx={ellipsisStyles}>
                      (CIP-129){" "}
                      <Typography
                        color="primary"
                        variant="body2"
                        component="span"
                      >
                        {cip129Identifier}
                      </Typography>
                    </Typography>
                    <img alt="" src={ICONS.copyBlueIcon} />
                  </ButtonBase>
                </Box>
              </Box>
            </Box>

            <Box sx={{ display: "flex", flex: { xl: 1 }, gap: 3 }}>
              <Box sx={{ width: { lg: "128px" } }}>
                <Typography
                  data-testid={`${view}-voting-power-label`}
                  variant="caption"
                  color="textSecondary"
                  sx={{ mb: 0.5 }}
                >
                  {t("votingPower")}
                </Typography>
                <Typography
                  data-testid={`${view}-voting-power`}
                  sx={{ whiteSpace: "nowrap" }}
                >
                  ₳ {correctDRepDirectoryFormat(votingPower)}
                </Typography>
              </Box>
              <Divider
                orientation="vertical"
                flexItem
                sx={({ palette }) => ({ borderColor: palette.lightBlue })}
              />
              <Box>
                <Typography
                  data-testid={`${view}-status-label`}
                  variant="caption"
                  color="textSecondary"
                  sx={{ mb: 0.5 }}
                >
                  {t("status")}
                </Typography>
                <Box display="flex" flexDirection="row">
                  <StatusPill
                    dataTestId={`${view}-${status}-pill`}
                    status={status}
                  />
                  {isMe && (
                    <StatusPill
                      dataTestId={`${view}-yourself-pill`}
                      status={DRepStatus.Yourself}
                      sx={{ ml: 0.75, display: { lg: "flex", xxs: "none" } }}
                    />
                  )}
                </Box>
              </Box>
            </Box>
          </Box>
        </Box>

        <Box
          display="flex"
          gap={2.5}
          sx={{
            "@container root (min-width: 480px)": {
              justifyContent: "flex-end",
              alignItems: "center",
            },
            minWidth: {
              xxs: "233px",
              xs: isConnected ? "233px" : "310px",
            },
          }}
        >
          {type === "DRep" && (
            <Button
              data-testid={`${view}-view-details-button`}
              variant="outlined"
              onClick={() =>
                navigate(
                  (isConnected
                    ? PATHS.dashboardDRepDirectoryDRep
                    : PATHS.dRepDirectoryDRep
                  ).replace(":dRepId", view),
                  { state: { enteredFromWithinApp: true } },
                )
              }
            >
              {t("viewDetails")}
            </Button>
          )}
          {status === "Active" &&
            isConnected &&
            onDelegate &&
            !isMyDrep &&
            !isInProgress && (
              <Button
                data-testid={`${view}-delegate-button`}
                onClick={onDelegate}
                isLoading={isDelegationLoading}
              >
                {t("delegate")}
              </Button>
            )}
          {status === "Active" && !isConnected && (
            <Button
              data-testid={`${view}-connect-to-delegate-button`}
              onClick={openChooseWalletModal}
            >
              {t("connectToDelegate")}
            </Button>
          )}
        </Box>
      </Box>
    </Card>
  );
};

const ellipsisStyles = {
  overflow: "hidden",
  textOverflow: "ellipsis",
  whiteSpace: "nowrap",
  maxWidth: { xxs: "200px", xs: "100%" },
} as const;
