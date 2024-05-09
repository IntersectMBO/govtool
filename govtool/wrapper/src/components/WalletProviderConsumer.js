"use client";

import { WalletProvider, useWalletContext } from "govtool-wallet-connector";

function Wallet() {
  const { enableWallet, disableWallet, walletAPI } = useWalletContext();
  return (
    <div>
      <button
        onClick={async () =>
          walletAPI ? await disableWallet() : await enableWallet("eternl")
        }
      >
        {`${walletAPI ? "disconnect" : "connect"} your wallet`}
      </button>
    </div>
  );
}

export function WalletConsumer() {
  return (
    <WalletProvider>
      <Wallet />
    </WalletProvider>
  );
}
