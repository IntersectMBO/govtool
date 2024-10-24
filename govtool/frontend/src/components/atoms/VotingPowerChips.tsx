import { Box, CircularProgress } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Typography, Tooltip } from "@atoms";
import { useScreenDimension, useTranslation } from "@hooks";
import { correctAdaFormat } from "@utils";

type VotingPowerChipsProps = {
  votingPower?: number;
  isLoading?: boolean;
  isShown?: boolean;
};

export const VotingPowerChips = ({
  isLoading,
  isShown,
  votingPower,
}: VotingPowerChipsProps) => {
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  return (
    isShown && (
      <Box
        data-testid="voting-power-chips"
        sx={{
          alignItems: "center",
          bgcolor: "textBlack",
          border: isMobile ? 2 : 0,
          borderColor: "#FBFBFF",
          borderRadius: 100,
          display: "flex",
          height: isMobile ? 16 : 24,
          px: 2,
          py: isMobile ? 1 : 1.5,
          maxHeight: "14px",
        }}
      >
        {!isMobile && (
          <Tooltip
            heading={t("tooltips.votingPower.heading")}
            paragraphOne={t("tooltips.votingPower.paragraphOne")}
            paragraphTwo={t("tooltips.votingPower.paragraphTwo")}
            placement="bottom-end"
            arrow
          >
            <InfoOutlinedIcon
              style={{
                color: "#ADAEAD",
                marginRight: "12px",
              }}
              fontSize="small"
            />
          </Tooltip>
        )}
        {screenWidth >= 1024 && (
          <Typography color="#A5A6A5" sx={{ mr: 1.5 }} variant="body2">
            {t("votingPower")}:
          </Typography>
        )}
        {isLoading ? (
          <CircularProgress size={20} color="primary" />
        ) : (
          <Typography
            data-testid="voting-power-chips-value"
            color="white"
            fontSize={18}
            fontWeight={600}
            sx={{ whiteSpace: "nowrap" }}
          >
            ₳ {correctAdaFormat(votingPower) ?? 0}
          </Typography>
        )}
      </Box>
    )
  );
};
