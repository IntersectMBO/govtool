import { config } from "dotenv";
config();

const CARDANO_API_METADATA_HOST_URL =
  process.env.CARDANOAPI_METADATA_URL ||
  "https://metadata-govtool.cardanoapi.io";
const SERVER_HOST_URL = process.env.HOST_URL || "http://localhost:8080";
const NETWORK = process.env.NETWORK || "preview";

const environments = {
  frontendUrl: SERVER_HOST_URL,
  blockfrostApiKey: process.env.BLOCKFROST_API_KEY,
  blockfrostApiUrl: "https://cardano-" + NETWORK + ".blockfrost.io/api",
  apiUrl: `${SERVER_HOST_URL}/api`,
  docsUrl: process.env.DOCS_URL || "https://docs.gov.tools/cardano-govtool",
  networkId: NETWORK === "mainnet" ? 1 : 0,
  faucet: {
    apiUrl: `https://faucet.${NETWORK}.world.dev.cardano.org`,
    address: process.env.FAUCET_ADDRESS,
    payment: { private: process.env.FAUCET_PAYMENT_PRIVATE },
    stake: { pkh: process.env.FAUCET_STAKE_PKH },
  },
  kuber: {
    apiUrl: `https://${NETWORK}.kuber.cardanoapi.io`,
    apiKey: process.env.KUBER_API_KEY || "",
  },
  txTimeOut: parseInt(process.env.TX_TIMEOUT) || 240000,
  metadataBucketUrl: `${CARDANO_API_METADATA_HOST_URL}/data`,
  lockInterceptorUrl: `${CARDANO_API_METADATA_HOST_URL}/lock`,
  ci: process.env.CI,
};

export default environments;
