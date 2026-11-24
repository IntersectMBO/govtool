import React, { ComponentProps, Suspense, useState } from "react";
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
import { Background, PageContainer } from "@/components/atoms";
import {
  Footer,
  GovernanceActionsHeader,
  ProposeActionBanner,
  TopNav,
} from "@/components/organisms";
import {
  type ForumProposeState,
  useProposeHeaderControls,
} from "@/components/organisms/ProposeHeaderControls";
import { PDF_PATHS } from "@/consts/paths";
import {
  useGetDRepVotingPowerList,
  useGetVoterInfo,
  useIsWalletConnected,
} from "@/hooks";
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
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { voter } = useGetVoterInfo();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();
  const { fetchDRepVotingPowerList } = useGetDRepVotingPowerList();
  const { username, setUsername } = useProposalDiscussion();
  const snackbarContext = useSnackbar();
  const { pathname } = useLocation();
  // The layout follows a saved wallet while it reconnects, as App.tsx does.
  const isConnected = useIsWalletConnected();
  const [forumPropose, setForumPropose] = useState<ForumProposeState | null>(
    null,
  );
  const proposeControls = useProposeHeaderControls(forumPropose);
  // Computed on every render: it also reads the saved wallet name.
  const walletStatus = getPdfWalletStatus(context);
  // The propose path is the list with pdf-ui's propose dialog over it.
  const isListPage =
    pathname === PDF_PATHS.proposalDiscussion ||
    pathname === PDF_PATHS.proposalDiscussionPropose;

  // The banner at the foot takes the same next step as the header button:
  // connect, sign in to the forum, create a display name, or propose.
  const onPropose = () => {
    proposeControls.onPropose();
    window.scrollTo({ top: 0, behavior: "smooth" });
  };

  const content = (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
        minHeight: !isConnected ? "100vh" : "auto",
      }}
    >
      {!isConnected && <TopNav />}
      <PageContainer
        component={isConnected ? "div" : "main"}
        sx={{ display: "flex", flex: 1, flexDirection: "column" }}
      >
        {isListPage && (
          <GovernanceActionsHeader
            action={proposeControls.action}
            notice={proposeControls.notice}
          />
        )}
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
            onProposeStateChange={setForumPropose}
            {...snackbarContext}
          />
        </Suspense>
        {isListPage && (
          <ProposeActionBanner onPropose={onPropose} />
        )}
      </PageContainer>
      {!isConnected && <Footer />}
    </Box>
  );

  // Public GovTool pages sit on the orange and blue background; the
  // dashboard layout draws its own.
  return isConnected ? content : <Background>{content}</Background>;
};
