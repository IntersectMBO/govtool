import React, { ComponentProps, Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import { useLocation } from "react-router";
import {
  useAppContext,
  useCardano,
  useGovernanceActions,
  useProposalDiscussion,
  useSnackbar,
} from "@/context";
import { useValidateMutation } from "@/hooks/mutations";
import { useScreenDimension } from "@/hooks/useScreenDimension";
import { Background } from "@/components/atoms";
import { Footer, TopNav } from "@/components/organisms";
import { useGetDRepVotingPowerList, useGetVoterInfo } from "@/hooks";
import {
  getAdaHolderVotingPower,
  getAccount,
  getEnactedProposalDetails,
} from "@/services";
import { env } from "@/config/env";
import { getPdfWalletStatus } from "@/utils/getPdfWalletStatus";

const ProposalDiscussion = React.lazy(() => import("@/pdf-ui/App"));

// Local and test runs serve metadata from host:port URLs, which pdf-ui
// rejects unless its URL validation is told to accept a port.
const isTestMode = ["development", "test"].includes(env.VITE_APP_ENV ?? "");

export const ProposalDiscussionPillar = () => {
  const { epochParams } = useAppContext();
  const { pagePadding } = useScreenDimension();
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { voter } = useGetVoterInfo();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();
  const { fetchDRepVotingPowerList } = useGetDRepVotingPowerList();
  const { username, setUsername } = useProposalDiscussion();
  const snackbarContext = useSnackbar();
  const { pathname } = useLocation();
  // Computed on every render: it also reads the saved wallet name.
  const walletStatus = getPdfWalletStatus(context);

  const content = (
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
          // Connected: the dashboard's PagePaddingBox values. Public: the
          // pagePadding every public GovTool page uses.
          px: context.isEnabled ? { xxs: 2, md: 5 } : pagePadding,
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
            pdfApiUrl={env.VITE_PDF_API_URL}
            walletAPI={{
              ...context,
              ...walletApi,
              createGovernanceActionJsonLD,
              createHash,
              voter,
            }}
            walletStatus={walletStatus}
            pathname={pathname}
            validateMetadata={
              validateMetadata as ComponentProps<
                typeof ProposalDiscussion
              >["validateMetadata"]
            }
            fetchDRepVotingPowerList={fetchDRepVotingPowerList}
            username={username}
            setUsername={setUsername}
            epochParams={epochParams}
            allowUrlPorts={isTestMode}
            getAdaHolderVotingPower={getAdaHolderVotingPower}
            getAccount={getAccount}
            getEnactedProposalDetails={getEnactedProposalDetails}
            {...snackbarContext}
          />
        </Suspense>
      </Box>
      {!context.isEnabled && <Footer />}
    </Box>
  );

  // Public GovTool pages sit on the orange and blue background; the
  // dashboard layout draws its own.
  return context.isEnabled ? content : <Background>{content}</Background>;
};
