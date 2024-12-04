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
  pdfUrl: process.env.PDF_URL || "https://dev.api.pdf.gov.tools",
  networkId: parseInt(process.env.NETWORK_ID) || 0,
  faucet: {
    apiUrl:`https://faucet.${NETWORK}.world.dev.cardano.org`,
    apiKey: process.env.FAUCET_API_KEY || "",
    address:
      process.env.FAUCET_ADDRESS ||
      "addr_test1vz0ua2vyk7r4vufmpqh5v44awg8xff26hxlwyrt3uc67maqtql3kl",
  },
  kuber: {
    apiUrl: `https://${NETWORK}.kuber.cardanoapi.io`,
    apiKey: process.env.KUBER_API_KEY || "",
  },
  txTimeOut: parseInt(process.env.TX_TIMEOUT) || 240000,
  metadataBucketUrl: `${CARDANO_API_METADATA_HOST_URL}/data`,
  lockInterceptorUrl: `${CARDANO_API_METADATA_HOST_URL}/lock`,
  ci: process.env.CI,
  proposalFaucet: {
    payment: {
      private: process.env.PROPOSAL_FAUCET_PAYMENT_PRIVATE,
    },
    stake: {
      private: process.env.PROPOSAL_FAUCET_STAKE_PRIVATE,
    },
  },
};

export default environments;
