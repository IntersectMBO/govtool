import { Box } from "@mui/material";

import {
  ActiveActionsList,
  ActivityFeed,
  ActivityOverview,
  Footer,
  RecentlyRatified,
  TopNav,
} from "@organisms";

/** Governance Activity: headline figures, the activity feed and the live list. */
export const GovernanceActivity = () => (
  <Box sx={{ backgroundColor: "#FCFCFA", display: "flex", flexDirection: "column", minHeight: "100vh", overflowX: "clip" }}>
    <TopNav />
    <Box component="main" sx={{ flex: 1 }}>
      <ActivityOverview />
      <ActivityFeed />
      <RecentlyRatified />
      <ActiveActionsList />
    </Box>
    <Footer />
  </Box>
);
