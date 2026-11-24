import { Box } from "@mui/material";
import { useLocation } from "react-router";

import { Background, PageContainer } from "@atoms";
import { USER_PATHS } from "@consts";
import { useIsWalletConnected } from "@hooks";
import {
  Footer,
  GovernanceActionHistoryDetails,
  GovernanceActionHistoryList,
  GovernanceActionHistorySearchFiltersSortBar,
  GovernanceActionsHeader,
  TopNav,
} from "@organisms";

const GOVERNANCE_ACTION_SEGMENT = "actions/history/";

const GovernanceActionHistoryListPage = () => (
  <Box display="flex" flexDirection="column" flexGrow={1}>
    <GovernanceActionsHeader />
    <GovernanceActionHistorySearchFiltersSortBar>
      <GovernanceActionHistoryList />
    </GovernanceActionHistorySearchFiltersSortBar>
  </Box>
);

const GovernanceActionHistoryContent = () => {
  const { pathname } = useLocation();

  if (pathname.includes(GOVERNANCE_ACTION_SEGMENT)) {
    // The record's CIP-129 id; an old txHash#index link is rewritten by
    // getCurrentUrl before this renders.
    const id = pathname.split("/").pop();
    if (id) return <GovernanceActionHistoryDetails id={id} />;
  }

  // Reserved for the user's votes and favourites; nothing is built yet.
  if (pathname.startsWith(USER_PATHS.governanceActionsVotedByMe)) return null;

  return <GovernanceActionHistoryListPage />;
};

export const GovernanceActionHistoryPage = () => {
  const isConnected = useIsWalletConnected();

  return (
    <Background>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          flex: 1,
          minHeight: !isConnected ? "100vh" : "auto",
          // An outcome's header tint spans the window; clip keeps sticky intact.
          overflowX: "clip",
        }}
      >
        {!isConnected && <TopNav />}
        <PageContainer component={isConnected ? "div" : "main"} sx={{ display: "flex", flex: 1 }}>
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
        {!isConnected && <Footer />}
      </Box>
    </Background>
  );
};
