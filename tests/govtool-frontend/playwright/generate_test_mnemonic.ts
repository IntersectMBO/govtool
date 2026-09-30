import { generateMnemonic } from "bip39";

// A 24-word mnemonic; every test wallet is an HD account derived from it.
console.log("\n📋 Please copy the following to your environment variables:");
console.log(`TEST_WALLET_MNEMONIC="${generateMnemonic(256)}"`);
