import type { WalletOption } from "@molecules";

/**
 * Retrieves the CIP-95 wallets from the window.cardano object.
 * @returns {Array} An array of CIP-95 wallets, each containing the icon, label, and name.
 */
export const getCip95Wallets = (cardanoWindowObject: typeof window.cardano) => {
  const walletNames = Object.keys(cardanoWindowObject);
  const resultWallets: WalletOption[] = [];
  walletNames.forEach((walletName: string) => {
    const { icon, name, supportedExtensions } = cardanoWindowObject[walletName];
    if (icon && name && supportedExtensions) {
      // Check if the name already exists in resultWallets
      const isNameDuplicate = resultWallets.some(
        (wallet) => wallet.label === name,
      );
      // Check if the supportedExtensions array contains an entry with cip === 95
      const isCip95Available = Boolean(
        supportedExtensions?.some((extension) => extension.cip === 95),
      );
      // If the name is not a duplicate and cip === 95 is available, add it to resultWallets
      if (!isNameDuplicate && isCip95Available) {
        resultWallets.push({
          icon,
          label: name,
          name: walletName,
          cip95Available: true,
        });
      }
    }
  });
  return resultWallets;
};
