import { useCallback, useEffect } from "react";
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
import { useWalletConnectionListener } from "@hooks";
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
  Home,
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
  isWalletOptionalPath,
} from "@utils";
import { TopBanners } from "./components/organisms/TopBanners";
import { DashboardHome } from "./pages/DashboardHome";
import { GovernanceActionHistoryPage } from "./pages/GovernanceActionHistory";

export default () => {
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const { enable, isEnabled } = useCardano();
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

      if (!isOnAllowedPage) {
        navigate(PATHS.home);
      }
      cleanUpWalletData();
    };

    const isOnAllowedPage = isWalletOptionalPath(window.location.pathname);

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

  return (
    <TopBanners>
      <ScrollToTop />
      {currentUrl ? (
        <Navigate to={currentUrl} replace state={state} />
      ) : (
        <Routes>
          <Route path={PATHS.home} element={<Home />} />
          {/* One URL per page: the wallet picks the dashboard or the public
          layout. */}
          <Route element={<Dashboard />}>
            <Route path={PATHS.dashboard} element={<DashboardHome />} />
            {isEnabled && (
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
                  element={<GovernanceActionsIndex isConnected />}
                />
                <Route
                  path={PATHS.governanceActionsAction}
                  element={<DashboardGovernanceActionDetails />}
                />
                <Route element={<DRepDirectory isConnected />}>
                  <Route
                    path={PATHS.dRepDirectory}
                    element={<DRepDirectoryContent isConnected />}
                  />
                  <Route
                    path={PATHS.dRepDirectoryDRep}
                    element={<DRepDetails isConnected />}
                  />
                </Route>
              </>
            )}
          </Route>
          {!isEnabled && (
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
                element={<GovernanceActionsIndex />}
              />
              <Route
                path={PATHS.governanceActionsAction}
                element={<GovernanceActionDetails />}
              />
              <Route element={<DRepDirectory />}>
                <Route
                  path={PATHS.dRepDirectory}
                  element={<DRepDirectoryContent />}
                />
                <Route
                  path={PATHS.dRepDirectoryDRep}
                  element={<DRepDetails />}
                />
              </Route>
            </>
          )}
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
