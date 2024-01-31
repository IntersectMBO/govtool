import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";
import CheckIcon from "@mui/icons-material/Check";
import InfoOutlinedIcon from "@mui/icons-material/InfoOutlined";

import { Button, VotePill, Typography } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension } from "@hooks";
import { VotedProposal } from "@models";
import { theme } from "@/theme";
import {
  formatDisplayDate,
  getFullGovActionId,
  getProposalTypeLabel,
  getShortenedGovActionId,
  openInNewTab,
} from "@utils";
import { Tooltip } from "@atoms";
import { tooltips } from "@/consts/texts";

interface Props {
  votedProposal: VotedProposal;
  searchPhrase?: string;
  inProgress?: boolean;
}

export const GovernanceVotedOnCard = ({ votedProposal, inProgress }: Props) => {
  const navigate = useNavigate();
  const { proposal, vote } = votedProposal;
  const {
    palette: { lightBlue },
  } = theme;
  const { isMobile } = useScreenDimension();

  const proposalTypeNoEmptySpaces = getProposalTypeLabel(proposal.type).replace(
    / /g,
    ""
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
            "In progress"
          ) : (
            <>
              <CheckIcon fontSize="small" sx={{ marginRight: 0.5 }} />
              Vote submitted
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
          <Typography color={"#8E908E"} variant="caption">
            Governance Action Type:
          </Typography>
          <Box display={"flex"}>
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
          <Typography color={"#8E908E"} variant="caption">
            Governance Action ID:
          </Typography>
          <Box display={"flex"} mt={0.5}>
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
                  proposal.index
                )}-id`}
                variant="caption"
              >
                {getShortenedGovActionId(proposal.txHash, proposal.index)}
              </Typography>
            </Box>
          </Box>
        </Box>
        <Box data-testid="my-vote" mt={5}>
          <Typography color={"#8E908E"} variant="caption">
            My Vote:
          </Typography>
          <Box
            mt={1}
            px={0.5}
            py={0.5}
            display="flex"
            flexDirection="row"
            border={1}
            borderColor={"rgba(214, 226, 255, 1)"}
            borderRadius={100}
            flex={1}
            alignItems={"center"}
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
              Vote transaction
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
            Submission date:
          </Typography>
          <Typography fontWeight={600} variant="caption">
            {formatDisplayDate(proposal.createdDate)}
          </Typography>
          <Tooltip
            heading={tooltips.submissionDate.heading}
            paragraphOne={tooltips.submissionDate.paragraphOne}
            placement={"bottom-end"}
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
            Expiry date:
          </Typography>
          <Typography variant="caption" fontWeight={600}>
            {formatDisplayDate(proposal.expiryDate)}
          </Typography>
          <Tooltip
            heading={tooltips.expiryDate.heading}
            paragraphOne={tooltips.expiryDate.paragraphOne}
            paragraphTwo={tooltips.expiryDate.paragraphTwo}
            placement={"bottom-end"}
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
        bgcolor={"white"}
        px={isMobile ? 2 : 5}
        py={2}
        sx={{ borderBottomLeftRadius: 20, borderBottomRightRadius: 20 }}
      >
        <Button
          disabled={inProgress}
          data-testid={`govaction-${getFullGovActionId(
            proposal.txHash,
            proposal.index
          )}-change-your-vote`}
          onClick={() =>
            navigate(
              PATHS.dashboard_governance_actions_action.replace(
                ":proposalId",
                getFullGovActionId(proposal.txHash, proposal.index)
              ),
              {
                state: {
                  ...proposal,
                  vote: vote.vote.toLowerCase(),
                },
              }
            )
          }
          sx={{
            backgroundColor: "#FBFBFF",
            width: "100%",
          }}
          variant="outlined"
        >
          Change your vote
        </Button>
      </Box>
    </Box>
  );
};
