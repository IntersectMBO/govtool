import { Box, Typography } from "@mui/material";

import { IMAGES } from "@consts";
import { VotePill } from "@atoms";
import { useTranslation } from "@hooks";
import { correctAdaFormat } from "@utils";

type Props = {
  dRepYesVotes: number;
  dRepNoVotes: number;
  dRepAbstainVotes: number;
  poolYesVotes: number;
  poolNoVotes: number;
  poolAbstainVotes: number;
  ccYesVotes: number;
  ccNoVotes: number;
  ccAbstainVotes: number;
};

const Vote = ({
  vote,
  value,
}: {
  vote: "yes" | "no" | "abstain";
  value: string | number;
}) => (
  <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
    <VotePill vote={vote} maxWidth={82} />
    <Typography
      fontSize="16px"
      sx={{
        marginLeft: "12px",
        wordBreak: "break-all",
      }}
    >
      {value}
    </Typography>
  </Box>
);

export const VotesSubmitted = ({
  dRepAbstainVotes,
  dRepNoVotes,
  dRepYesVotes,
  poolAbstainVotes,
  poolNoVotes,
  poolYesVotes,
  ccAbstainVotes,
  ccNoVotes,
  ccYesVotes,
}: Props) => {
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
          mb: 3,
        }}
      >
        {t("govActions.forGovAction")}
      </Typography>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          gap: "12px",
        }}
      >
        <Typography
          sx={{
            fontSize: "18px",
            fontWeight: "600",
            lineHeight: "24px",
          }}
        >
          {t("govActions.dReps")}
        </Typography>
        <Vote vote="yes" value={`₳ ${correctAdaFormat(dRepYesVotes)}`} />
        <Vote
          vote="abstain"
          value={`₳ ${correctAdaFormat(dRepAbstainVotes)}`}
        />
        <Vote vote="no" value={`₳ ${correctAdaFormat(dRepNoVotes)}`} />
        <Box
          sx={{
            display: "flex",
            flexDirection: "column",
            gap: "12px",
            mt: "24px",
          }}
        >
          <Typography
            sx={{
              fontSize: "18px",
              fontWeight: "600",
              lineHeight: "24px",
            }}
          >
            {t("govActions.sPos")}
          </Typography>
          <Vote vote="yes" value={poolYesVotes} />
          <Vote vote="abstain" value={poolAbstainVotes} />
          <Vote vote="no" value={poolNoVotes} />
          <Box
            sx={{
              display: "flex",
              flexDirection: "column",
              gap: "12px",
              mt: "24px",
            }}
          >
            <Typography
              sx={{
                fontSize: "18px",
                fontWeight: "600",
                lineHeight: "24px",
              }}
            >
              {t("govActions.ccCommittee")}
            </Typography>
            <Vote vote="yes" value={ccYesVotes} />
            <Vote vote="abstain" value={ccAbstainVotes} />
            <Vote vote="no" value={ccNoVotes} />
          </Box>
        </Box>
      </Box>
    </Box>
  );
};
