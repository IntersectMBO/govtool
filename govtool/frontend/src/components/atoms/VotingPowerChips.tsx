import { Box, CircularProgress } from "@mui/material";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Typography } from "@atoms";
import { useCardano } from "@context";
import {
  useGetAdaHolderVotingPowerQuery,
  useGetDRepVotingPowerQuery,
  useScreenDimension,
  useTranslation,
} from "@hooks";
import { correctAdaFormat } from "@utils";
import { Tooltip } from "@atoms";

export const VotingPowerChips = () => {
  const { dRep, stakeKey, isDrepLoading } = useCardano();
  const { dRepVotingPower, isDRepVotingPowerLoading } =
    useGetDRepVotingPowerQuery();
  const { votingPower, powerIsLoading } =
    useGetAdaHolderVotingPowerQuery(stakeKey);
  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  return (
    <Box
      bgcolor={"black"}
      px={2}
      py={isMobile ? 1 : 1.5}
      display={"flex"}
      border={isMobile ? 2 : 0}
      borderColor="#FBFBFF"
      borderRadius={100}
      alignItems="center"
      maxHeight={isMobile ? undefined : 48}
    >
      {dRep?.isRegistered && (
        <Tooltip
          heading={t("tooltips.votingPower.heading")}
          paragraphOne={t("tooltips.votingPower.paragraphOne")}
          paragraphTwo={t("tooltips.votingPower.paragraphTwo")}
          placement={"bottom-end"}
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
      {(dRep?.isRegistered && isDRepVotingPowerLoading) ||
      (!dRep?.isRegistered && powerIsLoading) ||
      isDrepLoading ? (
        <CircularProgress size={20} color="primary" />
      ) : (
        <Typography
          color={"white"}
          fontSize={18}
          fontWeight={600}
          sx={{ whiteSpace: "nowrap" }}
        >
          ₳{" "}
          {dRep?.isRegistered
            ? correctAdaFormat(dRepVotingPower) ?? 0
            : correctAdaFormat(votingPower) ?? 0}
        </Typography>
      )}
    </Box>
  );
};
