import { Box } from "@mui/material";
import { useLocation } from "react-router";

import { Background, PageContainer } from "@atoms";
import { USER_PATHS } from "@consts";
import { useCardano } from "@context";
import {
  Footer,
  GovernanceActionHistoryDetails,
  GovernanceActionHistoryList,
  GovernanceActionHistorySearchFiltersSortBar,
  GovernanceActionsHeader,
  TopNav,
} from "@organisms";

const GOVERNANCE_ACTION_SEGMENT = "governance_actions/history/";

const GovernanceActionHistoryListPage = () => (
  <Box display="flex" flexDirection="column" flexGrow={1}>
    <GovernanceActionsHeader />
    <GovernanceActionHistorySearchFiltersSortBar>
      <GovernanceActionHistoryList />
    </GovernanceActionHistorySearchFiltersSortBar>
  </Box>
);

const GovernanceActionHistoryContent = () => {
  const { pathname, hash } = useLocation();

  if (pathname.includes(GOVERNANCE_ACTION_SEGMENT)) {
    // Links carry the CIP-105 id, so `#index` arrives as the URL hash.
    const id = `${pathname.split("/").pop()}${hash}`;
    if (id) return <GovernanceActionHistoryDetails id={id} />;
  }

  // Reserved for the user's votes and favourites; nothing is built yet.
  if (pathname.startsWith(USER_PATHS.governanceActionsVotedByMe)) return null;

  return <GovernanceActionHistoryListPage />;
};

export const GovernanceActionHistoryPage = () => {
  const { isEnabled } = useCardano();

  return (
    <Background>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          flex: 1,
          minHeight: !isEnabled ? "100vh" : "auto",
          // An outcome's header tint spans the window; clip keeps sticky intact.
          overflowX: "clip",
        }}
      >
        {!isEnabled && <TopNav />}
        <PageContainer component={isEnabled ? "div" : "main"} sx={{ display: "flex", flex: 1 }}>
          <Box
            component="section"
            className="governance-actions-container"
            display="flex"
            flexDirection="column"
            flexGrow={1}
            minWidth={0}
          >
            <GovernanceActionHistoryContent />
          </Box>
        </PageContainer>
        {!isEnabled && <Footer />}
      </Box>
    </Background>
  );
};
