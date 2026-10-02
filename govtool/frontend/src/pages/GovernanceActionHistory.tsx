import { Box } from "@mui/material";
import { useLocation } from "react-router";

import { Background, Typography } from "@atoms";
import { USER_PATHS } from "@consts";
import { useCardano } from "@context";
import { useScreenDimension, useTranslation } from "@hooks";
import {
  Footer,
  GovernanceActionHistoryDetails,
  GovernanceActionHistoryList,
  GovernanceActionHistorySearchFiltersSortBar,
  TopNav,
} from "@organisms";

const GOVERNANCE_ACTION_SEGMENT = "governance_actions/history/";

const GovernanceActionHistoryListPage = () => {
  const { isEnabled } = useCardano();
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();

  return (
    <Box display="flex" flexDirection="column" flexGrow={1}>
      {!isEnabled && (
        <Typography
          sx={{ paddingX: 1, paddingY: 2 }}
          variant={isMobile ? "title1" : "headline3"}
          component="h1"
        >
          {t("governanceActionHistoryList.title")}
        </Typography>
      )}
      <GovernanceActionHistorySearchFiltersSortBar />
      <Box marginTop={3}>
        <GovernanceActionHistoryList />
      </Box>
    </Box>
  );
};

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
  const { pagePadding } = useScreenDimension();
  const { isEnabled } = useCardano();

  return (
    <Background>
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          flex: 1,
          minHeight: !isEnabled ? "100vh" : "auto",
        }}
      >
        {!isEnabled && <TopNav />}
        <Box
          sx={{
            px: isEnabled ? { xs: 2, sm: 5 } : pagePadding,
            py: 3,
            display: "flex",
            flex: 1,
          }}
        >
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
        </Box>
        {!isEnabled && <Footer />}
      </Box>
    </Background>
  );
};
