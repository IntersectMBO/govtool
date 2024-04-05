import { Box, Divider } from "@mui/material";

import { Button, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { AutomatedVotingCardProps } from "./types";
import { Card } from "./Card";
import { primaryBlue } from "@/consts";
import { useModal } from "@/context";

export const AutomatedVotingCard = ({
  description,
  inProgress,
  isConnected,
  isSelected,
  onClickDelegate,
  onClickInfo,
  title,
  votingPower,
}: AutomatedVotingCardProps) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { openModal } = useModal();
  const { t } = useTranslation();

  return (
    <Card
      {...(inProgress && {
        variant: "warning",
        label: t("inProgress"),
      })}
      {...(isSelected && {
        variant: "primary",
        label: "Selected",
      })}
      sx={{
        alignItems: "center",
        bgcolor: (theme) => `${theme.palette.neutralWhite}40`,
        boxShadow: `0px 4px 15px 0px ${primaryBlue.c100}`,
        display: "flex",
        flex: 1,
        flexDirection: screenWidth < 1440 ? "column" : "row",
        justifyContent: "space-between",
        mt: inProgress || isSelected ? 2 : 0,
        py: 2.25,
      }}
    >
      <Box
        sx={{
          flex: 1,
          mb: screenWidth < 1440 ? 1.5 : 0,
          width: screenWidth < 1440 ? "100%" : "auto",
        }}
      >
        <Typography>{title}</Typography>
        <Typography fontWeight={400} sx={{ mt: 0.5 }} variant="body2">
          {description}
        </Typography>
      </Box>
      <Divider
        flexItem
        orientation={screenWidth < 1440 ? "horizontal" : "vertical"}
        sx={{ ml: screenWidth < 1440 ? 0 : 1 }}
        variant={screenWidth < 1440 ? "fullWidth" : "middle"}
      />
      <Box
        sx={{
          alignContent: "flex-start",
          display: "flex",
          flexDirection: "column",
          px: screenWidth < 1440 ? 0 : 4.25,
          py: screenWidth < 1440 ? 1 : 0,
          width: screenWidth < 1440 ? "100%" : "auto",
        }}
      >
        <Typography color="neutralGray" fontWeight={500} variant="caption">
          {t("dRepDirectory.votingPower")}
        </Typography>
        <Typography sx={{ display: "flex", flexDirection: "row", mt: 0.5 }}>
          {'₳ '}
          {votingPower}
        </Typography>
      </Box>
      <Divider
        flexItem
        orientation={screenWidth < 1440 ? "horizontal" : "vertical"}
        sx={{ mr: screenWidth < 1440 ? 0 : 1 }}
        variant={screenWidth < 1440 ? "fullWidth" : "middle"}
      />
      <Box
        sx={{
          display: "flex",
          flexDirection: "row",
          gap: 2.5,
          mt: screenWidth < 1440 ? 3 : 0,
          width: screenWidth < 1440 ? "100%" : "auto",
        }}
      >
        <Button
          // TODO handle button click
          onClick={onClickInfo}
          size={isMobile ? "medium" : "large"}
          sx={{ flex: screenWidth < 768 ? 1 : undefined }}
          variant="outlined"
        >
          {t("info")}
        </Button>
        {!isConnected
          ? (
            <Button
              onClick={() => openModal({ type: "chooseWallet" })}
              size={isMobile ? "medium" : "large"}
              sx={{ flex: screenWidth < 768 ? 1 : undefined }}
            >
              {t("connectToDelegate")}
            </Button>
          )
          : !isSelected && (
            <Button
              onClick={onClickDelegate}
              size={isMobile ? "medium" : "large"}
              sx={{ flex: screenWidth < 768 ? 1 : undefined }}
            >
              {t("delegate")}
            </Button>
          )}
      </Box>
    </Card>
  );
};
