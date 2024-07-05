import React, { ComponentProps, Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import "@intersect.mbo/pdf-ui/style";
import { useCardano, useGovernanceActions } from "@/context";
import { useValidateMutation } from "@/hooks/mutations";
import { useScreenDimension } from "@/hooks/useScreenDimension";
import { Footer, TopNav } from "@/components/organisms";

const ProposalDiscussion = React.lazy(
  () => import("@intersect.mbo/pdf-ui/cjs"),
);

export const ProposalDiscussionPillar = () => {
  const { pagePadding } = useScreenDimension();
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();

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
          <ProposalDiscussion
            walletAPI={{
              ...context,
              ...walletApi,
              createGovernanceActionJsonLD,
              createHash,
            }}
            pathname={window.location.pathname}
            validateMetadata={
              validateMetadata as ComponentProps<
                typeof ProposalDiscussion
              >["validateMetadata"]
            }
          />
        </Suspense>
      </Box>
      {!context.isEnabled && <Footer />}
    </Box>
  );
};
