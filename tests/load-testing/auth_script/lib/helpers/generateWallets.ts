import { HdWallet } from "libcardano";

function createRange(start: number, end: number) {
  return Array.from({ length: end - start }, (_, i) => start + i);
}

const getAccount = async (index: number, wallet: HdWallet) => {
  const nthWallet = await wallet.getAccount(index);
  const singleAddrWallet = await nthWallet.singleAddressWallet();
  return singleAddrWallet;
};

export async function generateWallets() {
  const PEAK_USERS = parseInt(process.env.PEAK_USERS || "100");
  const wallet = await HdWallet.fromMnemonicString(
    "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon art"
  );
  return await Promise.all(createRange(0, PEAK_USERS).map(index => getAccount(index, wallet)));
}
