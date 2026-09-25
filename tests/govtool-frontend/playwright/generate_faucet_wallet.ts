import { Ed25519Key } from "libcardano";
import { ShelleyWallet } from "libcardano-wallet";
import environments from "./lib/constants/environments";

(async () => {
  try {
    console.log("\nGenerating your faucet wallet... 🔐");
    const payment = Ed25519Key.generate();
    const stake = Ed25519Key.generate();
    const address = new ShelleyWallet(payment, stake).addressBech32(
      environments.networkId
    );

    console.log("\n🎉 Wallet generated successfully!");
    console.log(
      "\n📋 Please copy the following to your environment variables:"
    );
    console.log(`FAUCET_ADDRESS=${address}`);
    console.log(`FAUCET_PAYMENT_PRIVATE=${payment.private.toString("hex")}`);
    console.log(`FAUCET_STAKE_PRIVATE=${stake.private.toString("hex")}`);

    console.log(
      "\n🎈 All set! Please ensure this wallet is funded with a sufficient balance"
    );
  } catch (error) {
    console.error("\n❌ An error occurred:", error.message);
  }
})();
