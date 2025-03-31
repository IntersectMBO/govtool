import { useCallback, useEffect } from "react";
import { Route, Routes, useNavigate } from "react-router-dom";

import { Modal, ScrollToTop } from "@atoms";
import {
  PATHS,
  PDF_PATHS,
  OUTCOMES_PATHS,
  USER_PATHS,
  BUDGET_DISCUSSION_PATHS,
} from "@consts";
import { useCardano, useFeatureFlag, useModal } from "@context";
import { useWalletConnectionListener } from "@hooks";
import {
  DashboardGovernanceActions,
  DashboardGovernanceActionDetails,
} from "@organisms";
import {
  ChooseStakeKey,
  CreateGovernanceAction,
  Dashboard,
  DashboardGovernanceActionsCategory,
  DRepDetails,
  DRepDirectory,
  DRepDirectoryContent,
  ErrorPage,
  GovernanceActionDetails,
  GovernanceActions,
  GovernanceActionsCategory,
  Home,
  RegisterAsdRep,
  RegisterAsDirectVoter,
  RetireAsDrep,
  RetireAsDirectVoter,
  EditDRepMetadata,
  ProposalDiscussionPillar,
} from "@pages";
import { SetupInterceptors } from "@services";
import {
  callAll,
  getItemFromLocalStorage,
  WALLET_LS_KEY,
  removeItemFromLocalStorage,
} from "@utils";
import { PublicRoute } from "./pages/PublicRoute";
import { TopBanners } from "./components/organisms/TopBanners";
import { DashboardHome } from "./pages/DashboardHome";
import { GovernanceActionOutComesPillar } from "./pages/GovernanceActionOutComes";

export default () => {
  const {
    isProposalDiscussionForumEnabled,
    isGovernanceOutcomesPillarEnabled,
  } = useFeatureFlag();
  const { enable, isEnabled } = useCardano();
  const navigate = useNavigate();
  const { modal, openModal, modals } = useModal();

  useWalletConnectionListener();

  useEffect(() => {
    SetupInterceptors(navigate);
  }, []);

  const checkTheWalletIsActive = useCallback(() => {
    const hrefCondition =
      window.location.pathname === PATHS.home ||
      window.location.pathname === PATHS.governanceActions ||
      window.location.pathname === PATHS.governanceActionsAction;

    const walletName = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
    if (window.cardano) {
      const walletExtensions = Object.keys(window.cardano);
      if (walletName && walletExtensions.includes(walletName)) {
        enable(walletName);
        return;
      }
    }
    if (
      (!window.cardano && walletName) ||
      (walletName && !Object.keys(window.cardano).includes(walletName))
    ) {
      if (!hrefCondition) {
        navigate(PATHS.home);
      }
      removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
      removeItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`);
    }
  }, []);

  useEffect(() => {
    checkTheWalletIsActive();
  }, [checkTheWalletIsActive]);

  return (
    <>
      <ScrollToTop />
      <TopBanners />
      <Routes>
        <Route path={PATHS.home} element={<Home />} />
        <Route path={PATHS.governanceActions} element={<GovernanceActions />} />
        <Route
          path={PATHS.governanceActionsCategory}
          element={<GovernanceActionsCategory />}
        />
        <Route
          path={PATHS.governanceActionsAction}
          element={<GovernanceActionDetails />}
        />
        {isProposalDiscussionForumEnabled && !isEnabled && (
          <>
            <Route
              path={`${PDF_PATHS.proposalDiscussion}/*`}
              element={<ProposalDiscussionPillar />}
            />
            <Route
              path={`${BUDGET_DISCUSSION_PATHS.budgetDiscussion}/*`}
              element={<ProposalDiscussionPillar />}
            />
          </>
        )}
        {isGovernanceOutcomesPillarEnabled && !isEnabled && (
          <>
            <Route
              path={`${OUTCOMES_PATHS.governanceActionsOutcomes}/*`}
              element={<GovernanceActionOutComesPillar />}
            />
            <Route
              path={USER_PATHS.governanceActionsVotedByMe}
              element={<GovernanceActionOutComesPillar />}
            />
          </>
        )}
        <Route element={<Dashboard />}>
          <Route path={PATHS.dashboard} element={<DashboardHome />} />
          {isProposalDiscussionForumEnabled && (
            <>
              <Route
                path={`${PDF_PATHS.proposalDiscussion}/*`}
                element={<ProposalDiscussionPillar />}
              />
              <Route
                path={`${BUDGET_DISCUSSION_PATHS.budgetDiscussion}/*`}
                element={<ProposalDiscussionPillar />}
              />
            </>
          )}
          {isGovernanceOutcomesPillarEnabled && (
            <>
              <Route
                path={`${OUTCOMES_PATHS.governanceActionsOutcomes}/*`}
                element={<GovernanceActionOutComesPillar />}
              />
              <Route
                path={USER_PATHS.governanceActionsVotedByMe}
                element={<GovernanceActionOutComesPillar />}
              />
            </>
          )}
          <Route
            path={PATHS.dashboardGovernanceActions}
            element={<DashboardGovernanceActions />}
          />
          <Route
            path={PATHS.dashboardGovernanceActionsAction}
            element={<DashboardGovernanceActionDetails />}
          />
          <Route
            path={PATHS.dashboardGovernanceActionsCategory}
            element={<DashboardGovernanceActionsCategory />}
          />
          <Route element={<DRepDirectory />}>
            <Route
              path={PATHS.dashboardDRepDirectory}
              element={<DRepDirectoryContent isConnected />}
            />
            <Route
              path={PATHS.dashboardDRepDirectoryDRep}
              element={<DRepDetails isConnected />}
            />
          </Route>
        </Route>
        <Route
          element={
            <PublicRoute>
              <DRepDirectory />
            </PublicRoute>
          }
        >
          <Route
            path={PATHS.dRepDirectory}
            element={<DRepDirectoryContent />}
          />
          <Route path={PATHS.dRepDirectoryDRep} element={<DRepDetails />} />
        </Route>
        <Route
          path={PATHS.createGovernanceAction}
          element={<CreateGovernanceAction />}
        />
        <Route path={PATHS.registerAsdRep} element={<RegisterAsdRep />} />
        <Route path={PATHS.retireAsDrep} element={<RetireAsDrep />} />
        <Route
          path={PATHS.registerAsDirectVoter}
          element={<RegisterAsDirectVoter />}
        />
        <Route
          path={PATHS.retireAsDirectVoter}
          element={<RetireAsDirectVoter />}
        />
        <Route path={PATHS.stakeKeys} element={<ChooseStakeKey />} />
        <Route path={PATHS.editDrepMetadata} element={<EditDRepMetadata />} />
        <Route path="*" element={<ErrorPage />} />
        <Route path={PATHS.error} element={<ErrorPage />} />
      </Routes>
      {modals[modal.type]?.component && (
        <Modal
          open={Boolean(modals[modal.type].component)}
          handleClose={
            !modals[modal.type].preventDismiss
              ? callAll(modals[modal.type]?.onClose, () =>
                  openModal({ type: "none", state: null }),
                )
              : undefined
          }
        >
          {modals[modal.type].component!}
        </Modal>
      )}
    </>
  );
};
