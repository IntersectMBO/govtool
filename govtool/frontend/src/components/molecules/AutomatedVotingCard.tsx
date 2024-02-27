import { Box, Divider } from "@mui/material";

import { AutomatedVotingCardProps } from "./types";
import { Button, Spacer, Typography } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";

export const AutomatedVotingCard = ({
  description,
  onClickDelegate,
  onClickInfo,
  title,
  votingPower,
}: AutomatedVotingCardProps) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  return (
    <Box
      sx={{
        alignItems: "center",
        bgcolor: "#FFFFFF4D",
        borderRadius: 4,
        boxShadow: `0px 4px 15px 0px #DDE3F5`,
        display: "flex",
        flex: 1,
        flexDirection: screenWidth < 1440 ? "column" : "row",
        justifyContent: "space-between",
        padding: "18px 24px",
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
          <Typography fontWeight={400}>₳</Typography>
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
          mt: screenWidth < 1440 ? 3 : 0,
          width: screenWidth < 1440 ? "100%" : "auto",
        }}
      >
        <Button
          onClick={onClickInfo}
          size={isMobile ? "medium" : "large"}
          sx={{ flex: screenWidth < 768 ? 1 : undefined }}
          variant="outlined"
        >
          {t("info")}
        </Button>
        <Spacer x={2.5} />
        <Button
          onClick={onClickDelegate}
          size={isMobile ? "medium" : "large"}
          sx={{ flex: screenWidth < 768 ? 1 : undefined }}
        >
          {t("delegate")}
        </Button>
      </Box>
    </Box>
  );
};
