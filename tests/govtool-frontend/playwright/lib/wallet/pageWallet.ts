import { Page } from "@playwright/test";
import { SimpleCip30Wallet } from "libcardano-wallet";
import { TestWallet } from "./testWallets";

/** The name the app sees in window.cardano; the wallet-connect tests use it. */
export const TEST_WALLET_NAME = "demos";

const BINDING = "__govtoolTestWallet";

export type PageWalletOptions = {
  /**
   * Preselect the wallet and mark the network notice as seen, as after a user
   * has connected once, so the app connects on load. Default true.
   */
  autoConnect?: boolean;
  /** CIP extensions the wallet offers. [] makes a wallet without CIP-95. */
  supportedExtensions?: { cip: number }[];
  /** Stake keys (hex) reported as registered in addition to the wallet's own. */
  extraRegisteredPubStakeKeys?: string[];
  /** Reward addresses (hex) reported in addition to the wallet's own. */
  extraRewardAddresses?: string[];
};

/**
 * Gives the page a CIP-30 wallet, with CIP-95 unless turned off. The wallet and
 * its keys stay in the test process: the page only gets a proxy whose calls
 * come back here through a Playwright binding.
 */
export async function connectTestWallet(
  page: Page,
  wallet: SimpleCip30Wallet | TestWallet,
  {
    autoConnect = true,
    supportedExtensions = [{ cip: 95 }],
    extraRegisteredPubStakeKeys = [],
    extraRewardAddresses = [],
  }: PageWalletOptions = {}
): Promise<void> {
  const signer = "signer" in wallet ? wallet.signer : wallet;
  const api = signer.toProtableCip30();
  const cip95 = supportedExtensions.some(({ cip }) => cip === 95);
  const rewardAddress =
    "rewardAddress" in wallet
      ? wallet.rewardAddress
      : (await api.getRewardAddresses())[0];

  const methods: Record<string, (...args: any[]) => Promise<unknown>> = {
    getExtensions: async () => supportedExtensions,
    getNetworkId: () => api.getNetworkId(),
    getUtxos: () => api.getUtxos(),
    getCollateral: () => api.getCollateral(),
    getUsedAddresses: () => api.getUsedAddresses(),
    getUnusedAddresses: () => api.getUnusedAddresses(),
    getChangeAddress: () => api.getChangeAddress(),
    getRewardAddresses: async () => [
      ...(await api.getRewardAddresses()),
      ...extraRewardAddresses,
    ],
    signTx: (tx: string, partial?: boolean) => api.signTx(tx, partial),
    signData: (address: string, payload: string) =>
      api.signData(address, payload),
    submitTx: (tx: string) => api.submitTx(tx),
    ...(cip95 && {
      "cip95.getPubDRepKey": () => api.cip95!.getPubDRepKey(),
      "cip95.getRegisteredPubStakeKeys": async () => [
        ...(await api.cip95!.getRegisteredPubStakeKeys()),
        ...extraRegisteredPubStakeKeys,
      ],
      "cip95.getUnregisteredPubStakeKeys": () =>
        api.cip95!.getUnregisteredPubStakeKeys(),
      "cip95.signTx": (tx: string, partial?: boolean) =>
        api.cip95!.signTx(tx, partial),
      "cip95.signData": (address: string, payload: string) =>
        api.cip95!.signData(address, payload),
    }),
  };

  await page.exposeBinding(BINDING, (_source, method: string, args: unknown[]) => {
    const fn = methods[method];
    if (!fn) throw new Error(`Test wallet does not implement ${method}`);
    return fn(...args);
  });

  await page.addInitScript(
    ({
      binding,
      name,
      methodNames,
      autoConnect,
      supportedExtensions,
      rewardAddress,
    }) => {
      const call =
        (method: string) =>
        (...args: unknown[]) =>
          (window as any)[binding](method, args);
      const api: Record<string, any> = {};
      for (const method of methodNames) {
        const [first, second] = method.split(".");
        if (second) (api[first] ??= {})[second] = call(method);
        else api[first] = call(method);
      }
      const cardano = ((window as any).cardano ??= {});
      cardano[name] = {
        name,
        apiVersion: "1.0.0",
        // The wallet picker lists only wallets with an icon.
        icon: "data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg'/%3E",
        supportedExtensions,
        isEnabled: async () => true,
        enable: async () => api,
      };
      if (autoConnect && !localStorage.getItem("wallet_data_name")) {
        localStorage.setItem("wallet_data_name", JSON.stringify(name));
        // The stake key the user picked when they first connected. The home
        // page only redirects a connected wallet to the dashboard when it is
        // already stored.
        if (rewardAddress) {
          localStorage.setItem("wallet_data_stake_key", JSON.stringify(rewardAddress));
        }
        // The one-time network notice a connected user has already seen.
        localStorage.setItem(`network_info_${name}`, "true");
      }
    },
    {
      binding: BINDING,
      name: TEST_WALLET_NAME,
      methodNames: Object.keys(methods),
      autoConnect,
      supportedExtensions,
      rewardAddress,
    }
  );
}
