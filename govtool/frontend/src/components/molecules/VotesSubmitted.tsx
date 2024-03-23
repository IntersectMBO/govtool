import { Box, Typography } from "@mui/material";

import { IMAGES } from "@consts";
import { VotePill } from "@atoms";
import { useTranslation } from "@hooks";
import { correctAdaFormat } from "@/utils/adaFormat";

interface Props {
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
}

export const VotesSubmitted = ({ yesVotes, noVotes, abstainVotes }: Props) => {
  const { t } = useTranslation();

  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
      }}
    >
      <img
        alt="ga icon"
        src={IMAGES.govActionListImage}
        width="64px"
        height="64px"
        style={{ marginBottom: "24px" }}
      />
      <Typography
        sx={{
          fontSize: "22px",
          fontWeight: "600",
          lineHeight: "28px",
        }}
      >
        {t("govActions.voteSubmitted")}
      </Typography>
      <Typography
        sx={{
          fontSize: "22px",
          fontWeight: "500",
          lineHeight: "28px",
          mb: 1,
        }}
      >
        {t("govActions.forGovAction")}
      </Typography>
      <Typography color="textGray" fontSize="14px" sx={{ mb: 3 }}>
        {t("govActions.votesSubmittedOnChain")}
      </Typography>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          gap: "12px",
        }}
      >
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
          <VotePill vote="yes" maxWidth={82} />
          <Typography
            fontSize="16px"
            sx={{
              marginLeft: "12px",
              wordBreak: "break-all",
            }}
          >
            ₳ {correctAdaFormat(yesVotes)}
          </Typography>
        </Box>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
          <VotePill vote="abstain" maxWidth={82} />
          <Typography
            sx={{
              marginLeft: "12px",
              wordBreak: "break-all",
            }}
          >
            ₳ {correctAdaFormat(abstainVotes)}
          </Typography>
        </Box>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
          <VotePill vote="no" maxWidth={82} />
          <Typography
            sx={{
              marginLeft: "12px",
              wordBreak: "break-all",
            }}
          >
            ₳ {correctAdaFormat(noVotes)}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};
