import { useCallback, useEffect, useReducer } from "react";
import {
  Navigate,
  Route,
  Routes,
  useNavigate,
  useLocation,
} from "react-router";

import { Modal, ScrollToTop } from "@atoms";
import {
  PATHS,
  PDF_PATHS,
  GOV_ACTION_HISTORY_PATHS,
  USER_PATHS,
  BUDGET_DISCUSSION_PATHS,
} from "@consts";
import { useCardano, useFeatureFlag, useModal } from "@context";
import { useIsWalletConnected, useWalletConnectionListener } from "@hooks";
import { DashboardGovernanceActionDetails } from "@organisms";
import {
  ChooseStakeKey,
  CreateGovernanceAction,
  Dashboard,
  DRepDetails,
  DRepDirectory,
  DRepDirectoryContent,
  ErrorPage,
  GovernanceActionDetails,
  GovernanceActionsIndex,
  GovernanceActivity,
  Home,
  NotificationPreferencesPage,
  NotificationsPage,
  RegisterAsdRep,
  RegisterAsDirectVoter,
  RetireAsDrep,
  RetireAsDirectVoter,
  EditDRepMetadata,
  ProposalDiscussionPillar,
  BudgetProposalsArchive,
} from "@pages";
import { SetupInterceptors } from "@services";
import {
  callAll,
  getItemFromLocalStorage,
  WALLET_LS_KEY,
  removeItemFromLocalStorage,
  getCurrentUrl,
  getPageTitle,
  isWalletOnlyPath,
} from "@utils";
import { TopBanners } from "./components/organisms/TopBanners";
import { DashboardHome } from "./pages/DashboardHome";
import { GovernanceActionHistoryPage } from "./pages/GovernanceActionHistory";

export default () => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { enable } = useCardano();
  // A saved wallet counts while it reconnects, so a reload does not flash
  // the public layout before the dashboard.
  const isWalletConnected = useIsWalletConnected();
  // Re-renders once a saved wallet is dropped, as that changes no state.
  const [, rerender] = useReducer((count: number) => count + 1, 0);
  const navigate = useNavigate();
  const { modal, openModal, modals } = useModal();
  const { pathname, search, hash, state } = useLocation();
  // Links from before the URLs named resources are sent to the current one.
  const currentUrl = getCurrentUrl({ pathname, search, hash });

  useWalletConnectionListener();

  useEffect(() => {
    SetupInterceptors(navigate);
  }, []);

  useEffect(() => {
    document.title = getPageTitle(pathname, search);
  }, [pathname, search]);

  const checkTheWalletIsActive = useCallback(() => {
    const isWalletAvailable = () =>
        window.cardano && walletName && Object.keys(window.cardano).includes(walletName);

    const cleanUpWalletData = () => {
      removeItemFromLocalStorage(`${WALLET_LS_KEY}_name`);
      removeItemFromLocalStorage(`${WALLET_LS_KEY}_stake_key`);
      rerender();
    };

    const waitForWalletExtension = async () => {
      const timeout = 5000;
      const interval = 100;
      const startTime = Date.now();

      while (Date.now() - startTime < timeout) {
        if (isWalletAvailable()) {
          enable(walletName);
          return;
        }
        // eslint-disable-next-line no-await-in-loop
        await new Promise((resolve) => {
          setTimeout(resolve, interval);
        });
      }

      if (isWalletOnlyPath(window.location.pathname)) {
        navigate(PATHS.home);
      }
      cleanUpWalletData();
    };

    const walletName = getItemFromLocalStorage(`${WALLET_LS_KEY}_name`);

    if (!walletName) return;

    if (isWalletAvailable()) {
      enable(walletName);
    } else {
      waitForWalletExtension();
    }
  }, []);

  useEffect(() => {
    checkTheWalletIsActive();
  }, [checkTheWalletIsActive]);

  // Pages with both layouts; the rest need a wallet or have no dashboard.
  const walletOptionalRoutes = (
    <>
      {isProposalDiscussionForumEnabled && (
        <Route
          path={`${PDF_PATHS.proposalDiscussion}/*`}
          element={<ProposalDiscussionPillar />}
        />
      )}
      <Route
        path={`${BUDGET_DISCUSSION_PATHS.budgetDiscussion}/*`}
        element={<BudgetProposalsArchive />}
      />
      <Route
        path={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/*`}
        element={<GovernanceActionHistoryPage />}
      />
      <Route
        path={USER_PATHS.governanceActionsVotedByMe}
        element={<GovernanceActionHistoryPage />}
      />
      <Route
        path={PATHS.governanceActions}
        element={<GovernanceActionsIndex isConnected={isWalletConnected} />}
      />
      <Route
        path={PATHS.governanceActionsAction}
        element={
          isWalletConnected ? (
            <DashboardGovernanceActionDetails />
          ) : (
            <GovernanceActionDetails />
          )
        }
      />
      <Route element={<DRepDirectory isConnected={isWalletConnected} />}>
        <Route
          path={PATHS.dRepDirectory}
          element={<DRepDirectoryContent isConnected={isWalletConnected} />}
        />
        <Route
          path={PATHS.dRepDirectoryDRep}
          element={<DRepDetails isConnected={isWalletConnected} />}
        />
      </Route>
    </>
  );

  return (
    <TopBanners>
      <ScrollToTop />
      {currentUrl ? (
        <Navigate to={currentUrl} replace state={state} />
      ) : (
        <Routes>
          <Route path={PATHS.home} element={<Home />} />
          {/* One public page for everyone, with or without a wallet. */}
          <Route
            path={PATHS.governanceActivity}
            element={<GovernanceActivity />}
          />
          {/* One URL per page: the wallet picks the dashboard or the public
          layout of the same routes. */}
          <Route element={<Dashboard />}>
            <Route path={PATHS.dashboard} element={<DashboardHome />} />
            <Route
              path={PATHS.notifications}
              element={<NotificationsPage />}
            />
            <Route
              path={PATHS.notificationPreferences}
              element={<NotificationPreferencesPage />}
            />
            {isWalletConnected && walletOptionalRoutes}
          </Route>
          {!isWalletConnected && walletOptionalRoutes}
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
          <Route
            path={PATHS.editDrepMetadata}
            element={<EditDRepMetadata />}
          />
          <Route path="*" element={<ErrorPage />} />
          <Route path={PATHS.error} element={<ErrorPage />} />
        </Routes>
      )}
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
    </TopBanners>
  );
};
