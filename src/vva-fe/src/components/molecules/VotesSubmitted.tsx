import { IMAGES } from "@/consts";
import { Box, Typography } from "@mui/material";

import { theme } from "@/theme";
import { VotePill } from "@atoms";
import { useScreenDimension } from "@/hooks";
import { correctAdaFormat } from "@/utils/adaFormat";

interface Props {
  yesVotes: number;
  noVotes: number;
  abstainVotes: number;
}

export const VotesSubmitted = ({ yesVotes, noVotes, abstainVotes }: Props) => {
  const {
    palette: { lightBlue },
  } = theme;
  const { isMobile } = useScreenDimension();

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
        style={{ marginBottom: "10px" }}
      />
      <Typography fontSize={"22px"} fontWeight={"600"}>
        Votes Submitted
      </Typography>
      <Typography fontSize={"22px"} fontWeight={"500"}>
        for this Governance Action
      </Typography>
      <Typography
        color={"textGray"}
        fontSize={"14px"}
        sx={{ marginTop: "8px" }}
      >
        Votes submitted on-chain by DReps, SPOs and Constitutional Committee
        members.
      </Typography>
      <Typography
        color="neutralGray"
        variant="caption"
        sx={{
          marginTop: "40px",
        }}
      >
        Votes:
      </Typography>
      <Box
        sx={{
          border: `1px solid ${lightBlue}`,
          borderRadius: "20px",
          display: "flex",
          flexDirection: "column",
          padding: isMobile ? "16px 12px" : "32px 24px",
        }}
      >
        <Box
          sx={{ alignItems: "center", display: "flex", marginBottom: "24px" }}
        >
          <VotePill vote="yes" maxWidth={82} />
          <Typography
            fontSize={"16px"}
            sx={{
              marginLeft: "12px",
              whiteSpace: "nowrap",
            }}
          >
            ₳ {correctAdaFormat(yesVotes)}
          </Typography>
        </Box>
        <Box
          sx={{ alignItems: "center", display: "flex", marginBottom: "24px" }}
        >
          <VotePill vote="abstain" maxWidth={82} />
          <Typography
            sx={{
              marginLeft: "12px",
              whiteSpace: "nowrap",
            }}
          >
            ₳ {correctAdaFormat(abstainVotes)}
          </Typography>
        </Box>
        <Box sx={{ alignItems: "center", display: "flex" }}>
          <VotePill vote="no" maxWidth={82} />
          <Typography
            sx={{
              marginLeft: "12px",
              whiteSpace: "nowrap",
            }}
          >
            ₳ {correctAdaFormat(noVotes)}
          </Typography>
        </Box>
      </Box>
    </Box>
  );
};
