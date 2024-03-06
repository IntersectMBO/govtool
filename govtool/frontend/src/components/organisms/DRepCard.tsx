import { useNavigate } from "react-router-dom";
import { Box, ButtonBase, Divider } from "@mui/material";

import { useTranslation } from "@hooks";
import { Button, StatusPill, Typography } from "@atoms";
import { Card } from "@molecules";
import { correctAdaFormat } from "@/utils";
import { ICONS } from "@/consts";

export const DRepCard = ({
  isConnected,
  name,
  id,
  votingPower,
  status,
}: any) => {
  const navigate = useNavigate();
  const { t } = useTranslation();

  return (
    <Card sx={{ container: "root / inline-size", py: 2.5 }}>
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
              <Typography sx={ellipsisStyles}>{name}</Typography>
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
                <Typography color="primary" variant="body2" sx={ellipsisStyles}>
                  {id}
                </Typography>
                <img alt="" src={ICONS.copyBlueIcon} />
              </ButtonBase>
            </Box>

            <Box display="flex" gap={3}>
              <Box maxWidth={100}>
                <Typography
                  variant="caption"
                  color="textSecondary"
                  sx={{ mb: 0.5 }}
                >
                  {t("votingPower")}
                </Typography>
                <Typography sx={{ whiteSpace: "nowrap" }}>
                  ₳
                  {' '}
                  {correctAdaFormat(votingPower)}
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
          <Button variant="outlined" onClick={() => navigate(`/drep/${id}`)}>
            {t("viewDetails")}
          </Button>
          {status === "active" && isConnected && (
            <Button>{t("delegate")}</Button>
          )}
          {status === "active" && !isConnected && (
            <Button>{t("connectToDelegate")}</Button>
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
