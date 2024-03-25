import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";
import CheckIcon from "@mui/icons-material/Check";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Button, VotePill, Typography, Tooltip } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { VotedProposal } from "@models";
import {
  formatDisplayDate,
  getFullGovActionId,
  getProposalTypeLabel,
  getShortenedGovActionId,
  openInNewTab,
} from "@utils";
import { theme } from "@/theme";

interface Props {
  votedProposal: VotedProposal;
  inProgress?: boolean;
}

export const GovernanceVotedOnCard = ({ votedProposal, inProgress }: Props) => {
  const navigate = useNavigate();
  const { proposal, vote } = votedProposal;
  const {
    palette: { lightBlue },
  } = theme;
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  const proposalTypeNoEmptySpaces = getProposalTypeLabel(proposal.type).replace(
    / /g,
    "",
  );

  return (
    <Box
      border={inProgress ? 1 : 0}
      borderColor="lightOrange"
      maxWidth={402}
      minWidth={isMobile ? 295 : 402}
      sx={{
        backgroundColor: "transparent",
        borderRadius: "20px",
        boxShadow: inProgress
          ? "2px 2px 20px 0px #F55A0033"
          : "0px 4px 15px 0px #DDE3F5",
        position: "relative",
      }}
      data-testid={`govaction-${proposalTypeNoEmptySpaces}-voted-on-card`}
    >
      <Box
        alignItems="center"
        bgcolor={inProgress ? "#F8ECD4" : "#E0F2DC"}
        border={1}
        borderColor={inProgress ? "#DEA029" : "#62BC52"}
        borderRadius={100}
        px={2.25}
        py={0.5}
        sx={{
          position: "absolute",
          top: -15,
          right: 20,
        }}
      >
        <Typography
          color={inProgress ? "#DEA029" : "#62BC52"}
          sx={{
            alignItems: "center",
            display: "flex",
            justifyContent: "center",
          }}
          variant="body2"
        >
          {inProgress ? (
            t("inProgress")
          ) : (
            <>
              <CheckIcon fontSize="small" sx={{ marginRight: 0.5 }} />
              {t("govActions.voteSubmitted")}
            </>
          )}
        </Typography>
      </Box>
      <Box
        pb={3}
        pt={4.25}
        px={3}
        sx={{
          borderTopLeftRadius: 20,
          borderTopRightRadius: 20,
          backgroundColor: "rgba(255, 255, 255, 0.3)",
        }}
      >
        <Box data-testid="governance-action-type">
          <Typography color="#8E908E" variant="caption">
            {t("govActions.governanceActionType")}
          </Typography>
          <Box display="flex">
            <Box
              mt={1.5}
              px={2.25}
              py={0.75}
              bgcolor={lightBlue}
              borderRadius={100}
            >
              <Typography
                data-testid={`${proposalTypeNoEmptySpaces}-type`}
                variant="caption"
              >
                {getProposalTypeLabel(proposal.type)}
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box mt={5}>
          <Typography color="#8E908E" variant="caption">
            {t("govActions.governanceActionId")}
          </Typography>
          <Box display="flex" mt={0.5}>
            <Box
              px={2.25}
              py={0.75}
              border={1}
              borderColor={lightBlue}
              borderRadius={100}
            >
              <Typography
                data-testid={`${getFullGovActionId(
                  proposal.txHash,
                  proposal.index,
                )}-id`}
                variant="caption"
              >
                {getShortenedGovActionId(proposal.txHash, proposal.index)}
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box data-testid="my-vote" mt={5}>
          <Typography color="#8E908E" variant="caption">
            {t("govActions.myVote")}
          </Typography>
          <Box
            mt={1}
            px={0.5}
            py={0.5}
            display="flex"
            flexDirection="row"
            border={1}
            borderColor="rgba(214, 226, 255, 1)"
            borderRadius={100}
            flex={1}
            alignItems="center"
          >
            <Box flex={1}>
              <VotePill vote={vote.vote} />
            </Box>
            <Button
              onClick={() =>
                openInNewTab("https://adanordic.com/latest_transactions")
              }
              variant="text"
              size="small"
              sx={{
                paddingY: 0.75,
                flex: 1,
                whiteSpace: "nowrap",
              }}
            >
              {t("govActions.voteTransaction")}
            </Button>
          </Box>
        </Box>
      </Box>
      {proposal.createdDate ? (
        <Box
          alignItems="center"
          bgcolor="#D6E2FF80"
          display="flex"
          flexDirection="row"
          justifyContent="center"
          py={0.75}
        >
          <Typography fontWeight={300} sx={{ mr: 1 }} variant="caption">
            {t("govActions.submissionDate")}
          </Typography>
          <Typography fontWeight={600} variant="caption">
            {formatDisplayDate(proposal.createdDate)}
          </Typography>
          <Tooltip
            heading={t("tooltips.submissionDate.heading")}
            paragraphOne={t("tooltips.submissionDate.paragraphOne")}
            placement="bottom-end"
            arrow
          >
            <InfoOutlinedIcon
              style={{
                color: "#ADAEAD",
              }}
              sx={{ ml: 0.7 }}
              fontSize="small"
            />
          </Tooltip>
        </Box>
      ) : null}
      {proposal.expiryDate ? (
        <Box
          data-testid="expiry-date"
          alignItems="center"
          bgcolor="rgba(247, 249, 251, 1)"
          display="flex"
          flexDirection="row"
          justifyContent="center"
          py={0.75}
        >
          <Typography fontWeight={300} sx={{ mr: 1 }} variant="caption">
            {t("govActions.expiryDate")}
          </Typography>
          <Typography variant="caption" fontWeight={600}>
            {formatDisplayDate(proposal.expiryDate)}
          </Typography>
          <Tooltip
            heading={t("tooltips.expiryDate.heading")}
            paragraphOne={t("tooltips.expiryDate.paragraphOne")}
            paragraphTwo={t("tooltips.expiryDate.paragraphTwo")}
            placement="bottom-end"
            arrow
          >
            <InfoOutlinedIcon
              style={{
                color: "#ADAEAD",
              }}
              sx={{ ml: 0.7 }}
              fontSize="small"
            />
          </Tooltip>
        </Box>
      ) : null}
      <Box
        bgcolor="white"
        px={isMobile ? 2 : 5}
        py={2}
        sx={{ borderBottomLeftRadius: 20, borderBottomRightRadius: 20 }}
      >
        <Button
          disabled={inProgress}
          data-testid={`govaction-${getFullGovActionId(
            proposal.txHash,
            proposal.index,
          )}-change-your-vote`}
          onClick={() =>
            navigate(
              PATHS.dashboardGovernanceActionsAction.replace(
                ":proposalId",
                getFullGovActionId(proposal.txHash, proposal.index),
              ),
              {
                state: {
                  ...proposal,
                  vote: vote.vote.toLowerCase(),
                },
              },
            )
          }
          sx={{
            backgroundColor: "#FBFBFF",
            width: "100%",
          }}
          variant="outlined"
        >
          {t("govActions.changeYourVote")}
        </Button>
      </Box>
    </Box>
  );
};
