// Make cardano wallet extension and make it available in window

import { mkCardanoWalletExtension } from "./cardanoWallet";

(async () => {
  const walletName = window["cardanoTestWallet"]["name"];

  if (walletName == null) {
    throw new Error(
      'Please specify the wallet name in the cardanoTestWallet.config object. Example: { walletName: "MyWallet" }',
    );
  }
  const extension = await mkCardanoWalletExtension();

  // @ts-ignore
  window.cardano = {
    demos: extension,
  };
})();
