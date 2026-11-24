import { useCallback, useState } from "react";
import { Box } from "@mui/material";

import { Button } from "@atoms";
import { SECURITY_RELEVANT_PARAMS_MAP } from "@consts";
import { useAppContext, useFeatureFlag } from "@context";
import { useTranslation } from "@hooks";
import { StatCard, VoteActionForm, VotesSubmitted } from "@molecules";
import { GovernanceActionDetailsCardData } from "@organisms";
import {
  MetadataIssue,
  MetadataValidationStatus,
  ProposalData,
  ProposalVote,
} from "@models";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { timeLeft } from "@/utils/governanceActionFormat";

type GovernanceActionDetailsCardProps = {
  isDashboard?: boolean;
  isDataMissing?: MetadataValidationStatus;
  metadataIssues?: MetadataIssue[];
  isInProgress?: boolean;
  isValidating?: boolean;
  /** The action's document is being read for its authors. */
  isDocumentLoading?: boolean;
  isVoter?: boolean;
  onMetadataRecovered?: () => void;
  vote?: ProposalVote | null;
  proposal: ProposalData;
};

const VOTE_PANEL_ID = "vote-panel";

const StatusPill = ({ tone, label }: { tone: keyof typeof PILL_TONES; label: string }) => (
  <Box
    component="span"
    data-testid="governance-action-status"
    sx={{
      alignItems: "center",
      backgroundColor: PILL_TONES[tone].bg,
      border: `1px solid ${PILL_TONES[tone].border}`,
      borderRadius: `${RADIUS.xs}px`,
      color: PILL_TONES[tone].text,
      display: "inline-flex",
      fontSize: 12,
      fontWeight: 500,
      gap: 0.5,
      lineHeight: "18px",
      px: 0.75,
    }}
  >
    <Box aria-hidden component="span" sx={{ backgroundColor: "currentColor", borderRadius: "50%", height: 6, width: 6 }} />
    {label}
  </Box>
);

/**
 * A governance action's page body: title block, the voting groups' totals and
 * the time left, the reasoning beside the action's information, and the vote
 * form for a connected voter.
 */
export const GovernanceActionDetailsCard = ({
  isDataMissing,
  metadataIssues,
  isInProgress,
  isVoter,
  isValidating,
  isDocumentLoading,
  onMetadataRecovered,
  vote,
  proposal,
}: GovernanceActionDetailsCardProps) => {
  const { t } = useTranslation();
  const [isVoteSubmitted, setIsVoteSubmitted] = useState<boolean>(false);
  const { areDRepVoteTotalsDisplayed } = useFeatureFlag();
  const { epochParams } = useAppContext();

  const isSecurityGroup = useCallback(
    () =>
      Object.values(SECURITY_RELEVANT_PARAMS_MAP).some(
        (paramKey) =>
          proposal.protocolParams?.[
            paramKey as keyof typeof proposal.protocolParams
          ] !== null,
      ),
    [proposal.protocolParams],
  );
  const showVoteForm =
    !!isVoter && areDRepVoteTotalsDisplayed(proposal.type, isSecurityGroup());
  const left = timeLeft(proposal.expiryDate);

  const statusPill = isInProgress ? (
    <StatusPill label={t("govActions.details.voteInProgress")} tone="warning" />
  ) : isVoteSubmitted ? (
    <StatusPill label={t("govActions.voteSubmitted")} tone="success" />
  ) : (
    <StatusPill label={t("govActions.details.votingOpen")} tone="success" />
  );

  const dataProps = {
    isDataMissing,
    isDocumentLoading,
    isValidating,
    metadataIssues,
    onMetadataRecovered,
    proposal,
  };

  return (
    <Box
      data-testid="governance-action-details-card"
      sx={{ display: "flex", flexDirection: "column", gap: 3, mt: 1.5, width: "100%" }}
    >
      <Box
        sx={{
          alignItems: { md: "flex-start" },
          display: "flex",
          flexDirection: { xxs: "column", md: "row" },
          gap: 2,
          justifyContent: "space-between",
        }}
      >
        <GovernanceActionDetailsCardData {...dataProps} section="header" statusPill={statusPill} />
        {showVoteForm && (
          <Button
            dataTestId="vote-on-action-button"
            onClick={() => {
              const panel = document.getElementById(VOTE_PANEL_ID);
              panel?.scrollIntoView({ behavior: "smooth", block: "start" });
              panel?.focus({ preventScroll: true });
            }}
            size="large"
            sx={{ alignSelf: { xxs: "flex-start", md: "center" }, flexShrink: 0 }}
          >
            {t("govActions.details.voteOnAction")}
          </Button>
        )}
      </Box>

      <Box
        sx={{
          alignItems: "stretch",
          display: "grid",
          gap: 2,
          gridTemplateColumns: { xxs: "1fr", lg: "minmax(0, 1fr) 240px" },
        }}
      >
        <VotesSubmitted type={proposal.type} votes={proposal} />
        <Box sx={{ display: "grid", gap: 2, gridTemplateColumns: { xxs: "1fr 1fr", lg: "1fr" } }}>
          <StatCard
            caption={
              proposal.expiryEpochNo !== undefined
                ? t("govActions.details.closesAtEpoch", { epoch: proposal.expiryEpochNo })
                : undefined
            }
            dataTestId="time-remaining-stat"
            label={t("govActions.details.timeRemaining")}
            value={left ?? "—"}
            valueColor={left ? ds.error["600"] : ds.gray["900"]}
          />
          {typeof epochParams?.epoch_no === "number" && (
            <StatCard
              dataTestId="current-epoch-stat"
              label={t("govActions.details.currentEpoch")}
              value={epochParams.epoch_no}
            />
          )}
        </Box>
      </Box>

      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "1fr", md: "minmax(0, 1fr) 360px" },
        }}
      >
        <GovernanceActionDetailsCardData {...dataProps} section="main" />
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
          {showVoteForm && (
            <Box
              aria-label={t("govActions.details.voteOnAction")}
              component="section"
              id={VOTE_PANEL_ID}
              tabIndex={-1}
              sx={{
                backgroundColor: "#FFFFFF",
                border: `1px solid ${isInProgress ? "#FFCBAD" : ds.gray["200"]}`,
                borderRadius: `${RADIUS.lg}px`,
                boxShadow: SHADOWS.card,
                p: { xxs: 2, md: 2.75 },
                scrollMarginTop: "96px",
                "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
              }}
            >
              <VoteActionForm
                isInProgress={isInProgress}
                previousVote={vote}
                proposal={proposal}
                setIsVoteSubmitted={setIsVoteSubmitted}
              />
            </Box>
          )}
          <GovernanceActionDetailsCardData {...dataProps} section="side" />
        </Box>
      </Box>
    </Box>
  );
};
