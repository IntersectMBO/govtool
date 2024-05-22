const CARDANO_API_METADATA_HOST_URL =
  process.env.CARDANOAPI_METADATA_URL ||
  "https://metadata-govtool.cardanoapi.io";
const SERVER_HOST_URL = process.env.HOST_URL || "http://localhost:8080";

const environments = {
  frontendUrl: SERVER_HOST_URL,
  apiUrl: `${SERVER_HOST_URL}/api`,
  docsUrl: process.env.DOCS_URL || "https://docs.sanchogov.tools",
  networkId: parseInt(process.env.NETWORK_ID) || 0,
  faucet: {
    apiUrl:
      process.env.FAUCET_API_URL ||
      "https://faucet.sanchonet.world.dev.cardano.org",
    apiKey: process.env.FAUCET_API_KEY || "",
  },
  kuber: {
    apiUrl: process.env.KUBER_API_URL || "https://kuber-govtool.cardanoapi.io",
    apiKey: process.env.KUBER_API_KEY || "",
  },
  txTimeOut: parseInt(process.env.TX_TIMEOUT) || 240000,
  metadataBucketUrl: `${CARDANO_API_METADATA_HOST_URL}/data`,
  lockInterceptorUrl: `${CARDANO_API_METADATA_HOST_URL}/lock`,
};

export default environments;
