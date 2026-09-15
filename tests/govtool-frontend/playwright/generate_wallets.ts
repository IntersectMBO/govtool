import { writeFile } from "fs";
import { ShelleyWallet } from "./lib/helpers/crypto";
import extractDRepFromWallet from "./lib/helpers/shellyWallet";

async function generateWallets(num: number): Promise<ShelleyWallet[]> {
  const wallets: ShelleyWallet[] = [];

  for (let i = 0; i < num; i++) {
    wallets.push(await ShelleyWallet.generate());
  }
  return wallets;
}

function saveWallets(wallets: ShelleyWallet[]): void {
  const jsonWallets = [];
  for (let i = 0; i < wallets.length; i++) {
    const dRepId = extractDRepFromWallet(wallets[i]);
    const networkId = process.env.NETWORK === "mainnet" ? 1 : 0;

    jsonWallets.push({
      ...wallets[i].json(),
      address: wallets[i].addressBech32(networkId),
      dRepId,
    });
  }
  const jsonString = JSON.stringify(jsonWallets, null, 2);
  writeFile("lib/_mock/wallets.json", jsonString, "utf-8", (err) => {
    if (err) {
      throw new Error("Failed to write wallets into file");
    }
  });
}

// Get the number of wallets from command line arguments
const numWallets = parseInt(process.argv[2], 10);

if (isNaN(numWallets) || numWallets <= 0) {
  console.error("Please provide a valid number of wallets to generate.");
  process.exit(1);
}

(async () => {
  const wallets = await generateWallets(numWallets);
  saveWallets(wallets);
})();
