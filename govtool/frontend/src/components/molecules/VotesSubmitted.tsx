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
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
          <VotePill vote="yes" maxWidth={82} />
          <Typography
            fontSize="16px"
            sx={{
              marginLeft: "12px",
              wordBreak: "break-all",
            }}
          >
            ₳ {correctAdaFormat(dRepYesVotes)}
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
            ₳ {correctAdaFormat(dRepAbstainVotes)}
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
            ₳ {correctAdaFormat(dRepNoVotes)}
          </Typography>
        </Box>
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
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}>
            <VotePill vote="yes" maxWidth={82} />
            <Typography
              fontSize="16px"
              sx={{
                marginLeft: "12px",
                wordBreak: "break-all",
              }}
            >
              ₳ {correctAdaFormat(poolYesVotes)}
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
              ₳ {correctAdaFormat(poolAbstainVotes)}
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
              ₳ {correctAdaFormat(poolNoVotes)}
            </Typography>
          </Box>
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
            <Box
              sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}
            >
              <VotePill vote="yes" maxWidth={82} />
              <Typography
                fontSize="16px"
                sx={{
                  marginLeft: "12px",
                  wordBreak: "break-all",
                }}
              >
                ₳ {correctAdaFormat(ccYesVotes)}
              </Typography>
            </Box>
            <Box
              sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}
            >
              <VotePill vote="abstain" maxWidth={82} />
              <Typography
                sx={{
                  marginLeft: "12px",
                  wordBreak: "break-all",
                }}
              >
                ₳ {correctAdaFormat(ccAbstainVotes)}
              </Typography>
            </Box>
            <Box
              sx={{ alignItems: "center", display: "flex", flexWrap: "wrap" }}
            >
              <VotePill vote="no" maxWidth={82} />
              <Typography
                sx={{
                  marginLeft: "12px",
                  wordBreak: "break-all",
                }}
              >
                ₳ {correctAdaFormat(ccNoVotes)}
              </Typography>
            </Box>
          </Box>
        </Box>
      </Box>
    </Box>
  );
};
