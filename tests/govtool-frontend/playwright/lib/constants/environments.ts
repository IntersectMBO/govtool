import { config } from "dotenv";
config();

const CARDANO_API_METADATA_HOST_URL =
  process.env.CARDANOAPI_METADATA_URL ||
  "https://metadata-govtool.cardanoapi.io";
const SERVER_HOST_URL = process.env.HOST_URL || "http://localhost:8080";

const environments = {
  frontendUrl: SERVER_HOST_URL,
  apiUrl: `${SERVER_HOST_URL}/api`,
  docsUrl: process.env.DOCS_URL || "https://docs.gov.tools",
  pdfUrl: process.env.PDF_URL || "https://dev.api.pdf.gov.tools",
  networkId: parseInt(process.env.NETWORK_ID) || 0,
  faucet: {
    apiUrl:
      process.env.FAUCET_API_URL ||
      "https://faucet.sanchonet.world.dev.cardano.org",
    apiKey: process.env.FAUCET_API_KEY || "",
    address: process.env.FAUCET_ADDRESS || "addr_test1vz0ua2vyk7r4vufmpqh5v44awg8xff26hxlwyrt3uc67maqtql3kl",
  },
  kuber: {
    apiUrl: process.env.KUBER_API_URL || "https://kuber-govtool.cardanoapi.io",
    apiKey: process.env.KUBER_API_KEY || "",
  },
  txTimeOut: parseInt(process.env.TX_TIMEOUT) || 240000,
  metadataBucketUrl: `${CARDANO_API_METADATA_HOST_URL}/data`,
  lockInterceptorUrl: `${CARDANO_API_METADATA_HOST_URL}/lock`,
  ci: process.env.CI,
};

export default environments;
