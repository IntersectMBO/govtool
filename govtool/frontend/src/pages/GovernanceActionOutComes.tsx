import { Footer, TopNav } from "@/components/organisms";
import { useCardano } from "@/context";
import { useScreenDimension } from "@/hooks";
import Outcomes from "@intersect.mbo/govtool-outcomes-pillar-ui";
import { Box, CircularProgress } from "@mui/material";
import { Suspense } from "react";

export const GovernanceActionOutComesPillar = () => {
  const { pagePadding } = useScreenDimension();
  const { ...context } = useCardano();
  return (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
        minHeight: !context.isEnabled ? "100vh" : "auto",
      }}
    >
      {!context.isEnabled && <TopNav />}
      <Box
        sx={{
          px: context.isEnabled ? { xs: 2, sm: 5 } : pagePadding,
          py: 3,
          display: "flex",
          flex: 1,
        }}
      >
        <Suspense
          fallback={
            <Box
              sx={{
                display: "flex",
                flex: 1,
                alignItems: "center",
                justifyContent: "center",
              }}
            >
              <CircularProgress />
            </Box>
          }
        >
          <Outcomes description="This is a test for the outcomes package........" />
        </Suspense>
      </Box>
      {!context.isEnabled && <Footer />}
    </Box>
  );
};
