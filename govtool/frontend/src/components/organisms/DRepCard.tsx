import { useNavigate } from "react-router-dom";
import { Box, ButtonBase, Divider } from "@mui/material";

import { Button, StatusPill, Typography } from "@atoms";
import { ICONS, PATHS } from "@consts";
import { useModal, useSnackbar } from "@context";
import { useTranslation } from "@hooks";
import { DRepData } from "@models";
import { Card } from "@molecules";
import { correctAdaFormat } from "@utils";

type DRepCardProps = {
  dRep: DRepData;
  isConnected: boolean;
  isInProgress?: boolean;
  isMe?: boolean;
  onDelegate?: () => void;
};

export const DRepCard = ({
  dRep: { status, type, view, votingPower },
  isConnected,
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
        label: t("yourself"),
      })}
      {...(isInProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      sx={{ container: "root / inline-size", py: 2.5 }}
    >
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
            columnGap={6}
            sx={{
              "@container (min-width: 480px)": {
                flexDirection: "row",
                alignItems: "center",
              },
              containerType: "inline-size",
            }}
          >
            <Box flex={1} minWidth={0}>
              <Typography sx={ellipsisStyles}>{type}</Typography>
              <ButtonBase
                data-testid={`${view}-copy-id-button`}
                onClick={(e) => {
                  navigator.clipboard.writeText(view);
                  addSuccessAlert(t("alerts.copiedToClipboard"));
                  e.stopPropagation();
                }}
                sx={{
                  gap: 1,
                  maxWidth: "100%",
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

            <Box display="flex" gap={3}>
              <Box>
                <Typography
                  variant="caption"
                  color="textSecondary"
                  sx={{ mb: 0.5 }}
                >
                  {t("votingPower")}
                </Typography>
                <Typography sx={{ whiteSpace: "nowrap" }}>
                  ₳ {correctAdaFormat(votingPower)}
                </Typography>
              </Box>
              <Divider
                orientation="vertical"
                flexItem
                sx={({ palette }) => ({ borderColor: palette.lightBlue })}
              />
              <Box>
                <Typography
                  variant="caption"
                  color="textSecondary"
                  sx={{ mb: 0.5 }}
                >
                  {t("status")}
                </Typography>
                <StatusPill status={status} sx={{ width: 80 }} />
              </Box>
            </Box>
          </Box>
        </Box>

        <Box
          display="flex"
          flex={1}
          gap={2.5}
          minWidth={isConnected ? 233 : 310}
          sx={{
            "@container root (min-width: 480px)": {
              justifyContent: "flex-end",
            },
          }}
        >
          <Button
            data-testid={`${view}-view-details-button`}
            variant="outlined"
            onClick={() =>
              navigate(
                (isConnected
                  ? PATHS.dashboardDRepDirectoryDRep
                  : PATHS.dRepDirectoryDRep
                ).replace(":dRepId", view),
              )
            }
          >
            {t("viewDetails")}
          </Button>
          {status === "Active" &&
            isConnected &&
            onDelegate &&
            !isMe &&
            !isInProgress && (
              <Button
                data-testid={`${view}-delegate-button`}
                onClick={onDelegate}
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
