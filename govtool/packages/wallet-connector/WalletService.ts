import { TWalletAPI, WalletConnectorErrors } from "./types";

export namespace WalletService {
  export const enableWallet = async (
    walletName: string
  ): Promise<TWalletAPI> => {
    try {
      if (!window.cardano[walletName]?.supportedExtensions)
        throw new Error(
          WalletConnectorErrors.NO_CIP30_SUPPORT.replace(
            "%WALLET_NAME%",
            walletName
          )
        );

      if (
        !window.cardano[walletName]?.supportedExtensions.some(
          (item) => item.cip === 95
        )
      )
        throw new Error(
          WalletConnectorErrors.NO_CIP95_SUPPORT.replace(
            "%WALLET_NAME%",
            walletName
          )
        );

      const walletApi = await window.cardano[walletName]?.enable({
        extensions: [{ cip: 95 }],
      });

      return walletApi;
    } catch (e) {
      throw e;
    }
  };
}
