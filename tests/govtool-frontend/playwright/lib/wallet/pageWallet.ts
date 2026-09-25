import { Page } from "@playwright/test";
import { SimpleCip30Wallet } from "libcardano-wallet";

/** The name the app sees in window.cardano; the wallet-connect tests use it. */
export const TEST_WALLET_NAME = "demos";

const BINDING = "__govtoolTestWallet";

/**
 * Gives the page a CIP-30 wallet with the CIP-95 extension. The wallet and its
 * keys stay in the test process: the page only gets a proxy whose calls come
 * back here through a Playwright binding.
 *
 * With `autoConnect`, the app finds the wallet already chosen and the network
 * notice already seen, as after a user has connected once, so no stored login
 * state is needed.
 */
export async function connectTestWallet(
  page: Page,
  wallet: SimpleCip30Wallet,
  { autoConnect = true }: { autoConnect?: boolean } = {}
): Promise<void> {
  const api = wallet.toProtableCip30();

  const methods: Record<string, (...args: any[]) => Promise<unknown>> = {
    getExtensions: async () => [{ cip: 95 }],
    getNetworkId: () => api.getNetworkId(),
    getUtxos: () => api.getUtxos(),
    getCollateral: () => api.getCollateral(),
    getUsedAddresses: () => api.getUsedAddresses(),
    getUnusedAddresses: () => api.getUnusedAddresses(),
    getChangeAddress: () => api.getChangeAddress(),
    getRewardAddresses: () => api.getRewardAddresses(),
    signTx: (tx: string, partial?: boolean) => api.signTx(tx, partial),
    signData: (address: string, payload: string) =>
      api.signData(address, payload),
    submitTx: (tx: string) => api.submitTx(tx),
    "cip95.getPubDRepKey": () => api.cip95!.getPubDRepKey(),
    "cip95.getRegisteredPubStakeKeys": () =>
      api.cip95!.getRegisteredPubStakeKeys(),
    "cip95.getUnregisteredPubStakeKeys": () =>
      api.cip95!.getUnregisteredPubStakeKeys(),
    "cip95.signTx": (tx: string, partial?: boolean) =>
      api.cip95!.signTx(tx, partial),
    "cip95.signData": (address: string, payload: string) =>
      api.cip95!.signData(address, payload),
  };

  await page.exposeBinding(BINDING, (_source, method: string, args: unknown[]) => {
    const fn = methods[method];
    if (!fn) throw new Error(`Test wallet does not implement ${method}`);
    return fn(...args);
  });

  await page.addInitScript(
    ({ binding, name, methodNames, autoConnect }) => {
      const call =
        (method: string) =>
        (...args: unknown[]) =>
          (window as any)[binding](method, args);
      const api: Record<string, any> = { cip95: {} };
      for (const method of methodNames) {
        const [first, second] = method.split(".");
        if (second) api[first][second] = call(method);
        else api[first] = call(method);
      }
      const cardano = ((window as any).cardano ??= {});
      cardano[name] = {
        name,
        apiVersion: "1.0.0",
        icon: "",
        supportedExtensions: [{ cip: 95 }],
        isEnabled: async () => true,
        enable: async () => api,
      };
      if (autoConnect && !localStorage.getItem("wallet_data_name")) {
        localStorage.setItem("wallet_data_name", JSON.stringify(name));
        // The one-time network notice a connected user has already seen.
        localStorage.setItem(`network_info_${name}`, "true");
      }
    },
    {
      binding: BINDING,
      name: TEST_WALLET_NAME,
      methodNames: Object.keys(methods),
      autoConnect,
    }
  );
}
