import { useNavigate } from "react-router-dom";
import { Box, ButtonBase, Divider } from "@mui/material";

import { Button, StatusPill, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useModal, useSnackbar } from "@context";
import { useTranslation } from "@hooks";
import { DRepData, DRepStatus } from "@models";
import { Card } from "@molecules";
import { correctDRepDirectoryFormat } from "@utils";

type DRepCardProps = {
  dRep: DRepData;
  isConnected: boolean;
  isDelegationLoading?: boolean;
  isInProgress?: boolean;
  isMe?: boolean;
  onDelegate?: () => void;
};

export const DRepCard = ({
  dRep: { status, type, view, votingPower },
  isConnected,
  isDelegationLoading,
  isInProgress,
  isMe,
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

  return (
    <Card
      {...(isMe && {
        variant: "primary",
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
            <Box minWidth={0} display="flex" flexDirection="column">
              <Typography sx={ellipsisStyles}>
                {type === "SoleVoter" ? t("dRepDirectory.directVoter") : type}
              </Typography>
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
                  "&:hover": {
                    opacity: 0.6,
                    transition: "opacity 0.3s",
                  },
                }}
              >
                <Typography color="primary" variant="body2" sx={ellipsisStyles}>
                  {view}
                </Typography>
                <img alt="" src={ICONS.copyBlueIcon} />
              </ButtonBase>
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
          minWidth={isConnected ? 233 : 310}
          sx={{
            "@container root (min-width: 480px)": {
              justifyContent: "flex-end",
              alignItems: "center",
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
} as const;
