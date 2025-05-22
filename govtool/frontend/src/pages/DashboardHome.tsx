import { Box } from "@mui/material";

import { DashboardCards, HelpBuildGovTool, Socials } from "@organisms";
import { UsefulLinks } from "@/components/organisms/Home/UsefulLinks";
import { useScreenDimension } from "@/hooks";

export const DashboardHome = () => {
  const { screenWidth } = useScreenDimension();

  return (
    <Box
      sx={{
        flex: 1,
        display: "flex",
        flexDirection: "column",
        gap: 8,
        px: screenWidth < 640 ? 2 : 5,
        py: 3,
      }}
    >
      <DashboardCards />
      <UsefulLinks />
      <Socials my={0} />
      <HelpBuildGovTool my={0} />
    </Box>
  );
};
