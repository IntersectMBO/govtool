import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import { VotedProposal } from "@models";
import {
  formatDisplayDate,
  getFullGovActionId,
  getProposalTypeLabel,
} from "@utils";
import {
  GovernanceActionCardElement,
  GovernanceActionCardHeader,
  GovernanceActionCardMyVote,
  GovernanceActionCardStatePill,
  GovernanceActionsDatesBox,
} from "@molecules";

const mockedLongText =
  "Lorem ipsum dolor sit, amet consectetur adipisicing elit. Sit, distinctio culpa minus eaque illo quidem voluptates quisquam mollitia consequuntur ex, sequi saepe? Ad ex adipisci molestiae sed.";

interface Props {
  votedProposal: VotedProposal;
  isDataMissing: boolean;
  searchPhrase?: string;
  inProgress?: boolean;
}

export const GovernanceVotedOnCard = ({
  votedProposal,
  isDataMissing,
  inProgress,
}: Props) => {
  const navigate = useNavigate();
  const { proposal, vote } = votedProposal;

  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const proposalTypeNoEmptySpaces = getProposalTypeLabel(proposal.type).replace(
    / /g,
    "",
  );

  return (
    <Box
      sx={{
        width: screenWidth < 420 ? 290 : isMobile ? 324 : 350,
        height: "100%",
        position: "relative",
        display: "flex",
        flexDirection: "column",
        justifyContent: "space-between",
        boxShadow: "0px 4px 15px 0px #DDE3F5",
        borderRadius: "20px",
        backgroundColor: "rgba(255, 255, 255, 0.3)",
        border: inProgress ? "1px solid #FFCBAD" : "1px solid #C0E4BA",
      }}
      data-testid={`govaction-${proposalTypeNoEmptySpaces}-card`}
    >
      <GovernanceActionCardStatePill
        variant={inProgress ? "inProgress" : "voteSubmitted"}
      />
      <Box
        sx={{
          padding: "40px 24px 0",
        }}
      >
        <GovernanceActionCardHeader
          title={mockedLongText}
          isDataMissing={isDataMissing}
        />
        <GovernanceActionCardElement
          label={t("govActions.abstract")}
          text={mockedLongText}
          textVariant="twoLines"
          dataTestId="governance-action-abstract"
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("govActions.governanceActionType")}
          text={getProposalTypeLabel(proposal.type)}
          textVariant="pill"
          dataTestId={`${proposalTypeNoEmptySpaces}-type`}
          isSliderCard
        />
        <GovernanceActionsDatesBox
          createdDate={formatDisplayDate(proposal.createdDate)}
          expiryDate={formatDisplayDate(proposal.expiryDate)}
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("govActions.governanceActionId")}
          text={getFullGovActionId(proposal.txHash, proposal.index)}
          dataTestId={`${getFullGovActionId(
            proposal.txHash,
            proposal.index,
          )}-id`}
          isCopyButton
          isSliderCard
        />
        <GovernanceActionCardMyVote vote={vote.vote} />
      </Box>
      <Box
        bgcolor={"white"}
        px={isMobile ? 2 : 5}
        py={2}
        sx={{
          boxShadow: "0px 4px 15px 0px #DDE3F5",
          borderBottomLeftRadius: 20,
          borderBottomRightRadius: 20,
        }}
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
            width: "100%",
          }}
          variant="contained"
        >
          {t("govActions.viewDetails")}
        </Button>
      </Box>
    </Box>
  );
};
