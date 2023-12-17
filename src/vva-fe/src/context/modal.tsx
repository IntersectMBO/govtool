import { createContext, useContext, useMemo, useReducer } from "react";

import { Modal, type MuiModalChildren } from "@atoms";
import {
  ChooseWalletModal,
  ExternalLinkModal,
  StatusModal,
  VotingPowerModal,
} from "@organisms";
import { basicReducer, callAll, BasicReducer } from "@utils";

interface ProviderProps {
  children: React.ReactNode;
}

interface ContextModal {
  component: null | MuiModalChildren;
  variant?: "modal" | "popup";
  preventDismiss?: boolean;
  onClose?: () => void;
}

export type ModalType =
  | "none"
  | "chooseWallet"
  | "statusModal"
  | "externalLink"
  | "votingPower";

const modals: Record<ModalType, ContextModal> = {
  none: {
    component: null,
  },
  chooseWallet: {
    component: <ChooseWalletModal />,
  },
  statusModal: {
    component: <StatusModal />,
  },
  externalLink: {
    component: <ExternalLinkModal />,
  },
  votingPower: {
    component: <VotingPowerModal />,
  },
};

type Optional<T, K extends keyof T> = Pick<Partial<T>, K> & Omit<T, K>;

interface ModalState<T> {
  type: ModalType;
  state: T | null;
}

interface ModalContext<T> {
  modal: ContextModal;
  state: T | null;
  openModal: (modal: Optional<ModalState<T>, "state">) => void;
  closeModal: () => void;
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
const ModalContext = createContext<ModalContext<any>>({} as ModalContext<any>);
ModalContext.displayName = "ModalContext";

function ModalProvider<T>(props: ProviderProps) {
  const [modal, openModal] = useReducer<BasicReducer<ModalState<T>>>(
    basicReducer,
    {
      state: null,
      type: "none",
    }
  );

  const value = useMemo(
    () => ({
      modal: modals[modal.type],
      state: modal.state,
      openModal,
      closeModal: callAll(modals[modal.type]?.onClose, () =>
        openModal({ type: "none", state: null })
      ),
    }),
    [modal, openModal]
  );

  return (
    <ModalContext.Provider value={value} {...props}>
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
      {props.children}
    </ModalContext.Provider>
  );
}

function useModal<T>() {
  const context = useContext<ModalContext<T>>(ModalContext);
  if (context === undefined) {
    throw new Error("useModal must be used within a ModalProvider");
  }
  return context;
}

export { ModalProvider, useModal };
