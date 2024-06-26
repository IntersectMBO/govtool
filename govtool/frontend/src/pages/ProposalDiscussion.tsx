import React, { ComponentProps, Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import "@intersect.mbo/pdf-ui/style";
import { useCardano, useGovernanceActions } from "@/context";
import { useValidateMutation } from "@/hooks/mutations";
import { useScreenDimension } from "@/hooks/useScreenDimension";
import { TopNav } from "@/components/organisms";

const ProposalDiscussion = React.lazy(
  () => import("@intersect.mbo/pdf-ui/cjs"),
);

export const ProposalDiscussionPillar = () => {
  const { pagePadding } = useScreenDimension();
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();

  return (
    <Box>
      {!context.isEnabled && <TopNav />}
      <Box
        sx={{
          px: context.isEnabled ? { xs: 2, sm: 5 } : pagePadding,
          my: 3,
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
                height: "100vw",
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
    </Box>
  );
};
