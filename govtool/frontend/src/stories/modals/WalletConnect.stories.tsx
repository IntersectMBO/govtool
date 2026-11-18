import { useEffect } from "react";
import { Meta, StoryFn } from "@storybook/react-vite";
import { expect, screen } from "storybook/test";

import { Modal } from "@atoms";
import { WalletConnectionModal } from "@organisms";
import { callAll } from "@/utils";

import { ModalType, useModal } from "../../context/modal";

const LACE_ICON =
  "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAzMiAzMiI+PGNpcmNsZSBjeD0iMTYiIGN5PSIxNiIgcj0iMTQiIGZpbGw9IiM3QTVBRjgiLz48L3N2Zz4=";

const meta = {
  title: "Example/Modals/WalletConnect",
  component: WalletConnectionModal,
} satisfies Meta<typeof WalletConnectionModal>;

export default meta;

type Args = { type: ModalType; state?: unknown; withWallet?: boolean };

const Template: StoryFn<Args> = ({ type, state, withWallet }) => {
  const { openModal, modal, modals } = useModal();
  if (withWallet) {
    (window as unknown as { cardano?: unknown }).cardano = {
      lace: { icon: LACE_ICON, name: "lace", supportedExtensions: [{ cip: 95 }] },
    };
  }
  useEffect(() => {
    openModal({ type, state: state ?? null });
  }, [openModal]);
  return modals[modal.type]?.component ? (
    <Modal
      open
      handleClose={callAll(modals[modal.type]?.onClose, () => openModal({ type: "none", state: null }))}
    >
      {modals[modal.type].component!}
    </Modal>
  ) : (
    <span />
  );
};

export const ConnectWallet = Template.bind({});
ConnectWallet.args = { type: "chooseWallet", withWallet: true };
ConnectWallet.play = async () => {
  await expect(await screen.findByText("Connect your Wallet")).toBeVisible();
  await expect(screen.getByTestId("lace-wallet-button")).toBeVisible();
};

export const WrongNetwork = Template.bind({});
WrongNetwork.args = {
  type: "walletConnection",
  state: {
    view: "error",
    walletName: "Lace",
    error: "Error: You are trying to connect with a wallet connected to testnet. Please adjust your wallet settings to connect to mainnet or select a different wallet.",
  },
};
WrongNetwork.play = async () => {
  await expect(await screen.findByText("Switch to Cardano Mainnet")).toBeVisible();
};

export const Declined = Template.bind({});
Declined.args = { type: "walletConnection", state: { view: "error", walletName: "Lace", error: "user declined" } };
Declined.play = async () => {
  await expect(await screen.findByText("Connection request declined")).toBeVisible();
};

export const Checking = Template.bind({});
Checking.args = { type: "walletConnection", state: { view: "checking" } };
Checking.play = async () => {
  await expect(await screen.findByText("Checking your wallet")).toBeVisible();
};

export const Disconnect = Template.bind({});
Disconnect.args = { type: "disconnectWallet", state: { onConfirm: () => {} } };
Disconnect.play = async () => {
  await expect(await screen.findByTestId("confirm-disconnect-button")).toBeVisible();
};
