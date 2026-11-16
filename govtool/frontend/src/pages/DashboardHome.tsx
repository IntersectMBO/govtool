import { Box } from "@mui/material";

import { PageContainer } from "@atoms";
import { DashboardCards } from "@organisms";
import { UsefulLinks } from "@/components/organisms/UsefulLinks";

export const DashboardHome = () => (
  <PageContainer sx={{ display: "flex", flex: 1, flexDirection: "column", gap: 6 }}>
    <DashboardCards />
    <Box>
      <UsefulLinks />
    </Box>
  </PageContainer>
);
