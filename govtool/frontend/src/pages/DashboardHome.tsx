import { PageContainer } from "@atoms";
import { DashboardCards } from "@organisms";

// The dashboard places Useful Links itself: in its card column, or under the
// DRep dashboard.
export const DashboardHome = () => (
  <PageContainer sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 6 }}>
    <DashboardCards />
  </PageContainer>
);
