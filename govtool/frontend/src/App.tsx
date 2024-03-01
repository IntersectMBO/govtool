import { useCallback, useEffect } from "react";
import { Route, Routes, useNavigate } from "react-router-dom";

import { Modal, ScrollToTop } from "@atoms";
import { PATHS } from "@consts";
import { useCardano, useModal } from "@context";
import {
  DashboardCards,
  DashboardGovernanceActions,
  DashboardGovernanceActionDetails,
} from "@organisms";
import {
  ChooseStakeKey,
  Dashboard,
  ErrorPage,
  Home,
  GovernanceActions,
  DelegateTodRep,
  RegisterAsdRep,
  GovernanceActionDetails,
  UpdatedRepMetadata,
  GovernanceActionsCategory,
  DashboardGovernanceActionsCategory,
  RetireAsSoleVoter,
  DRepDirectory,
  DRepDirectoryContent,
} from "@pages";
import {
  callAll,
  getItemFromLocalStorage,
  WALLET_LS_KEY,
  removeItemFromLocalStorage,
} from "@utils";
import { SetupInterceptors } from "./services";
import { useGetVoterInfo, useWalletConnectionListener } from "./hooks";
import { RegisterAsSoleVoter } from "./pages/RegisterAsSoleVoter";

export default function App() {
  const { enable, setVoter, setIsDrepLoading } = useCardano();
  const navigate = useNavigate();
  const { data } = useGetVoterInfo();
  const { modal, openModal, modals } = useModal();

  useWalletConnectionListener();

  useEffect(() => {
    SetupInterceptors(navigate);
  }, []);

  useEffect(() => {
    setIsDrepLoading(true);
    setVoter(data);
    const timer = setTimeout(() => setIsDrepLoading(false), 1000);

    return () => clearTimeout(timer);
  }, [data?.isRegisteredAsDRep, data?.isRegisteredAsSoleVoter]);

  const checkTheWalletIsActive = useCallback(() => {
    const hrefCondition =
      window.location.pathname === PATHS.home ||
      window.location.pathname === PATHS.governance_actions ||
      window.location.pathname === PATHS.governance_actions_action;

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
        <Route
          path={PATHS.governance_actions}
          element={<GovernanceActions />}
        />
        <Route
          path={PATHS.governance_actions_category}
          element={<GovernanceActionsCategory />}
        />
        <Route
          path={PATHS.governance_actions_action}
          element={<GovernanceActionDetails />}
        />
        <Route element={<Dashboard />}>
          <Route path={PATHS.dashboard} element={<DashboardCards />} />
          <Route
            path={PATHS.dashboard_governance_actions}
            element={<DashboardGovernanceActions />}
          />
          <Route
            path={PATHS.dashboard_governance_actions_action}
            element={<DashboardGovernanceActionDetails />}
          />
          <Route
            path={PATHS.dashboard_governance_actions_category}
            element={<DashboardGovernanceActionsCategory />}
          />
          <Route
            path={PATHS.dashboard_drep_directory}
            element={<DRepDirectoryContent isConnected />}
          />
        </Route>
        <Route path={PATHS.drep_directory} element={<DRepDirectory />} />
        <Route path={PATHS.delegateTodRep} element={<DelegateTodRep />} />
        <Route path={PATHS.registerAsdRep} element={<RegisterAsdRep />} />
        <Route
          path={PATHS.registerAsSoleVoter}
          element={<RegisterAsSoleVoter />}
        />
        <Route path={PATHS.retireAsSoleVoter} element={<RetireAsSoleVoter />} />
        <Route path={PATHS.stakeKeys} element={<ChooseStakeKey />} />
        <Route path={PATHS.updateMetadata} element={<UpdatedRepMetadata />} />
        <Route path="*" element={<ErrorPage />} />
        <Route path={PATHS.error} element={<ErrorPage />} />
      </Routes>
      {modals[modal.type]?.component && (
        <Modal
          open={Boolean(modals[modal.type].component)}
          handleClose={
            !modals[modal.type].preventDismiss
              ? callAll(modals[modal.type]?.onClose, () =>
                  openModal({ type: "none", state: null })
                )
              : undefined
          }
        >
          {modals[modal.type]?.component ?? <></>}
        </Modal>
      )}
    </>
  );
}
