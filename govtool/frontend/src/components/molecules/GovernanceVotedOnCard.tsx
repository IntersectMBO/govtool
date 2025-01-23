import { useNavigate } from "react-router-dom";
import { Box } from "@mui/material";

import { Button } from "@atoms";
import { PATHS } from "@consts";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  encodeCIP129Identifier,
  getFullGovActionId,
  getProposalTypeLabel,
  getProposalTypeNoEmptySpaces,
} from "@utils";
import {
  GovernanceActionCardElement,
  GovernanceActionCardHeader,
  GovernanceActionCardMyVote,
  GovernanceActionCardStatePill,
  GovernanceActionsDatesBox,
} from "@molecules";
import { VotedProposal } from "@/models";

type Props = {
  votedProposal: VotedProposal;
  inProgress?: boolean;
};

export const GovernanceVotedOnCard = ({ votedProposal, inProgress }: Props) => {
  const navigate = useNavigate();
  const { proposal, vote } = votedProposal;
  const {
    abstract,
    createdDate,
    createdEpochNo,
    expiryDate,
    expiryEpochNo,
    index,
    metadataStatus,
    metadataValid,
    txHash,
    type,
    title,
  } = proposal;

  const { isMobile, screenWidth } = useScreenDimension();
  const { t } = useTranslation();

  const govActionId = getFullGovActionId(txHash, index);
  const cip129GovernanceActionId = encodeCIP129Identifier({
    txID: txHash,
    index: index.toString(16).padStart(2, "0"),
    bech32Prefix: "gov_action",
  });

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
        backgroundColor: !metadataValid
          ? "rgba(251, 235, 235, 0.50)"
          : "rgba(255, 255, 255, 0.3)",
        // TODO: To decide if voted on cards can be actually in progress
        border: inProgress
          ? "1px solid #FFCBAD"
          : !metadataValid
          ? "1px solid #F6D5D5"
          : "1px solid #C0E4BA",
      }}
      data-testid={`govaction-${getProposalTypeNoEmptySpaces(type)}-card`}
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
          title={title}
          isDataMissing={metadataStatus}
        />
        <GovernanceActionCardElement
          label={t("govActions.abstract")}
          text={abstract}
          textVariant="twoLines"
          dataTestId="governance-action-abstract"
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("govActions.governanceActionType")}
          text={getProposalTypeLabel(type)}
          textVariant="pill"
          dataTestId={`${getProposalTypeNoEmptySpaces(type)}-type`}
          isSliderCard
        />
        <GovernanceActionsDatesBox
          createdDate={createdDate}
          expiryDate={expiryDate}
          expiryEpochNo={expiryEpochNo}
          createdEpochNo={createdEpochNo}
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("govActions.cip129GovernanceActionId")}
          text={cip129GovernanceActionId}
          dataTestId={`${cip129GovernanceActionId}-id`}
          isCopyButton
          isSliderCard
        />
        <GovernanceActionCardElement
          label={t("govActions.governanceActionId")}
          text={govActionId}
          dataTestId={`${govActionId}-id`}
          isCopyButton
          isSliderCard
          isSemiTransparent
        />
        {vote && (
          <GovernanceActionCardMyVote
            voteTxHash={vote.txHash}
            vote={vote.vote}
          />
        )}
      </Box>
      <Box
        bgcolor="white"
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
          data-testid={`govaction-${govActionId}-change-your-vote`}
          onClick={() =>
            navigate(
              PATHS.dashboardGovernanceActionsAction.replace(
                ":proposalId",
                govActionId,
              ),
              {
                state: {
                  proposal,
                  vote,
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
