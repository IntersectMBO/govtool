import { Box, CircularProgress } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Typography, Tooltip } from "@atoms";
import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useGetDRepVotingPowerQuery,
  useGetVoterInfo,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { correctAdaFormat } from "@utils";

export const VotingPowerChips = () => {
  const { stakeKey, isEnableLoading } = useCardano();
  const { dRepVotingPower } = useGetDRepVotingPowerQuery();
  const { votingPower } = useGetAdaHolderVotingPowerQuery(stakeKey);
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();
  const { voter } = useGetVoterInfo();

  return (
    <Box
      sx={{
        alignItems: 'center',
        bgcolor: 'textBlack',
        border: isMobile ? 2 : 0,
        borderColor: '#FBFBFF',
        borderRadius: 100,
        display: 'flex',
        height: isMobile ? 16 : 24,
        px: 2,
        py: isMobile ? 1 : 1.5,
      }}
    >
      {voter?.isRegisteredAsDRep && (
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
          {t("votingPower")}
        </Typography>
      )}
      {(voter?.isRegisteredAsDRep && dRepVotingPower === undefined) ||
        (!voter?.isRegisteredAsDRep && votingPower === undefined) ||
        isEnableLoading ||
        !voter ? (
          <CircularProgress size={20} color="primary" />
      ) : (
        <Typography
          color="white"
          fontSize={18}
          fontWeight={600}
          sx={{ whiteSpace: "nowrap" }}
        >
          ₳{" "}
          {voter?.isRegisteredAsDRep
            ? correctAdaFormat(dRepVotingPower) ?? 0
            : correctAdaFormat(votingPower) ?? 0}
        </Typography>
      )}
    </Box>
  );
};
