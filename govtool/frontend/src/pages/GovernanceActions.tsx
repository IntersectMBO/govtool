import { useEffect } from "react";
import { useNavigate } from "react-router";
import { Box } from "@mui/material";

import { Background, PageContainer, ScrollToManage } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useDataActionsBar, useFeatureFlag } from "@context";
import {
  Footer,
  GovernanceActionsHeader,
  GovernanceActionsListLayout,
  GovernanceActionsToVote,
  TopNav,
} from "@organisms";
import { WALLET_LS_KEY, getItemFromLocalStorage } from "@utils";

export const GovernanceActions = () => {
  const { ...dataActionsBarProps } = useDataActionsBar();
  const { chosenSorting, setChosenSorting } = dataActionsBarProps;
  const { governanceActionsSort } = useFeatureFlag();
  const { isEnabled } = useCardano();
  const navigate = useNavigate();

  useEffect(() => {
    if (isEnabled && getItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`)) {
      navigate(PATHS.dashboardGovernanceActions);
    }
  }, [isEnabled]);

  // `MostYesVotes` is the key Koios refuses. A selection carried in from an
  // earlier page must not survive into a provider that cannot honour it.
  useEffect(() => {
    if (!chosenSorting) return;
    if (governanceActionsSort.isSelectionStale(chosenSorting)) {
      setChosenSorting(
        governanceActionsSort.fallbackSelection(chosenSorting) ?? "",
      );
    }
  }, [chosenSorting, setChosenSorting, governanceActionsSort]);

  return (
    <Background>
      <ScrollToManage />
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          justifyContent: "flex-start",
          minHeight: "100vh",
        }}
      >
        <TopNav />
        <PageContainer component="main" sx={{ flex: 1 }}>
          <Box sx={{ display: "flex", flexDirection: "column" }}>
            <GovernanceActionsHeader />
            <GovernanceActionsListLayout>
              <GovernanceActionsToVote onDashboard={false} />
            </GovernanceActionsListLayout>
          </Box>
        </PageContainer>
        <Footer />
      </Box>
    </Background>
  );
};
