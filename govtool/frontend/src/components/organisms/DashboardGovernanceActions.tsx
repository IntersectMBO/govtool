import { useState, useEffect } from "react";
import { Box, CircularProgress } from "@mui/material";
import { useLocation } from "react-router";

import { PageContainer } from "@atoms";
import { useCardano, useDataActionsBar } from "@context";
import { useGetVoterInfo, useTranslation } from "@hooks";
import {
  GovernanceActionsToVote,
  DashboardGovernanceActionsVotedOn,
} from "@organisms";
import { SegmentedTabs } from "@/components/molecules/SegmentedTabs";

import { GovernanceActionsHeader } from "./GovernanceActionsHeader";
import { GovernanceActionsListLayout } from "./GovernanceActionsListLayout";

export const DashboardGovernanceActions = () => {
  const { debouncedSearchText } = useDataActionsBar();
  const { voter } = useGetVoterInfo();
  const { t } = useTranslation();
  const { isEnableLoading } = useCardano();
  const { state } = useLocation();

  const [content, setContent] = useState<number>(
    state?.isVotedListOnLoad ? 1 : 0,
  );

  useEffect(() => {
    window.history.replaceState({}, document.title);
  }, []);

  const isVoter = voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter;

  return (
    <PageContainer sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
      <GovernanceActionsHeader />
      <GovernanceActionsListLayout showCounts={!isVoter}>
        {!voter || isEnableLoading ? (
          <Box sx={{ display: "flex", justifyContent: "center", py: 6 }}>
            <CircularProgress />
          </Box>
        ) : (
          <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
            {isVoter && (
              <SegmentedTabs
                idPrefix="dashboard-actions"
                label={t("govActions.title")}
                onChange={(next) => setContent(next === "voted" ? 1 : 0)}
                tabs={[
                  { value: "toVote", label: t("govActions.toVote"), dataTestId: "to-vote-tab" },
                  { value: "voted", label: t("govActions.votedOnByMe"), dataTestId: "voted-tab" },
                ]}
                value={content === 1 ? "voted" : "toVote"}
              />
            )}
            <Box
              {...(isVoter
                ? SegmentedTabs.panelProps("dashboard-actions", content === 1 ? "voted" : "toVote")
                : {})}
            >
              {content === 0 || !isVoter ? (
                <GovernanceActionsToVote onDashboard />
            ) : (
              <DashboardGovernanceActionsVotedOn
                searchPhrase={debouncedSearchText}
              />
            )}
            </Box>
          </Box>
        )}
      </GovernanceActionsListLayout>
    </PageContainer>
  );
};
