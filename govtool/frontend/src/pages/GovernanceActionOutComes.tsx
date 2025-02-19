import { Box, CircularProgress } from "@mui/material";
import React, { Suspense } from "react";
import { Footer, TopNav } from "@/components/organisms";
import { useCardano } from "@/context";
import { useScreenDimension } from "@/hooks";

const GovernanceActionsOutcomes = React.lazy(
  () => import("@intersect.mbo/govtool-outcomes-pillar-ui/dist/esm"),
);

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
          <GovernanceActionsOutcomes />
        </Suspense>
      </Box>
      {!context.isEnabled && <Footer />}
    </Box>
  );
};
