import React, { ComponentProps, Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import { useLocation, useNavigate } from "react-router";
import {
  useAppContext,
  useCardano,
  useGovernanceActions,
  useProposalDiscussion,
  useModal,
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
import { PDF_PATHS } from "@/consts/paths";
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
  const { validateMetadata } = useValidateMutation();
  const { walletApi, ...context } = useCardano();
  const { voter } = useGetVoterInfo();
  const { createGovernanceActionJsonLD, createHash } = useGovernanceActions();
  const { fetchDRepVotingPowerList } = useGetDRepVotingPowerList();
  const { username, setUsername } = useProposalDiscussion();
  const snackbarContext = useSnackbar();
  const { pathname } = useLocation();
  const navigate = useNavigate();
  const { openModal } = useModal();
  // Computed on every render: it also reads the saved wallet name.
  const walletStatus = getPdfWalletStatus(context);
  // The propose path is the list with pdf-ui's propose dialog over it.
  const isListPage =
    pathname === PDF_PATHS.proposalDiscussion ||
    pathname === PDF_PATHS.proposalDiscussionPropose;

  // Disconnected: connect first. Connected: open the forum's propose flow,
  // which needs a forum sign-in; its sign-in prompt sits by the forum's own
  // Propose button at the top of the list.
  const onPropose = () => {
    if (!context.isEnabled) {
      openModal({ type: "chooseWallet" });
      return;
    }
    navigate(PDF_PATHS.proposalDiscussionPropose);
    window.scrollTo({ top: 0, behavior: "smooth" });
  };

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
      <PageContainer
        component={context.isEnabled ? "div" : "main"}
        sx={{ display: "flex", flex: 1, flexDirection: "column" }}
      >
        {isListPage && <GovernanceActionsHeader />}
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
        {isListPage && (
          <ProposeActionBanner onPropose={onPropose} />
        )}
      </PageContainer>
      {!context.isEnabled && <Footer />}
    </Box>
  );

  // Public GovTool pages sit on the orange and blue background; the
  // dashboard layout draws its own.
  return context.isEnabled ? content : <Background>{content}</Background>;
};
