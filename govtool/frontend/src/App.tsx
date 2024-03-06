import { useCallback, useEffect } from "react";
import { Route, Routes, useNavigate } from "react-router-dom";

import { Modal, ScrollToTop } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import { useWalletConnectionListener } from "@hooks";
import {
  DashboardCards,
  DashboardGovernanceActions,
  DashboardGovernanceActionDetails,
} from "@organisms";
import {
  ChooseStakeKey,
  CreateGovernanceAction,
  Dashboard,
  DashboardGovernanceActionsCategory,
  DelegateTodRep,
  DRepDirectory,
  ErrorPage,
  GovernanceActionDetails,
  GovernanceActions,
  GovernanceActionsCategory,
  Home,
  RegisterAsdRep,
  RegisterAsSoleVoter,
  RetireAsDrep,
  RetireAsSoleVoter,
  EditDRepMetadata,
} from "@pages";
import { SetupInterceptors } from "@services";
import {
  callAll,
  getItemFromLocalStorage,
  WALLET_LS_KEY,
  removeItemFromLocalStorage,
} from "@utils";

export default () => {
  const { enable } = useCardano();
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
        <Route element={<Dashboard />}>
          <Route path={PATHS.dashboard} element={<DashboardCards />} />
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
          <Route
            path={PATHS.dashboardDRepDirectory}
            element={<DRepDirectory />}
          />
        </Route>
        <Route path={PATHS.dRepDirectory} element={<DRepDirectory />} />
        <Route
          path={PATHS.createGovernanceAction}
          element={<CreateGovernanceAction />}
        />
        <Route path={PATHS.delegateTodRep} element={<DelegateTodRep />} />
        <Route path={PATHS.registerAsdRep} element={<RegisterAsdRep />} />
        <Route path={PATHS.retireAsDrep} element={<RetireAsDrep />} />
        <Route
          path={PATHS.registerAsSoleVoter}
          element={<RegisterAsSoleVoter />}
        />
        <Route path={PATHS.retireAsSoleVoter} element={<RetireAsSoleVoter />} />
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
