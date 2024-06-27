import React, { ComponentProps, Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import "@intersect.mbo/pdf-ui/style";
import { useCardano, useGovernanceActions } from "@/context";
import { useValidateMutation } from "@/hooks/mutations";

const ProposalDiscussion = React.lazy(
  () => import("@intersect.mbo/pdf-ui/cjs"),
);

export const PDFWrapper = () => {
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();

  return (
    <Box
      sx={{
        px: { xs: 2, sm: 5 },
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
  );
};
