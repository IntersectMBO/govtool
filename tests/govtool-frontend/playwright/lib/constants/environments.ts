const environments = {
  frontendUrl: process.env.HOST_URL || "http://localhost:5173",
  apiUrl: `${process.env.HOST_URL}/api` || "http://localhost:9999",
  docsUrl: process.env.DOCS_URL || "https://docs.sanchogov.tools",
  networkId: parseInt(process.env.NETWORK_ID) || 0,
  oneTimeWalletSetup: process.env.ONE_TIME_WALLET_SETUP === "true" || false,
  faucet: {
    apiUrl:
      process.env.FAUCET_API_URL ||
      "https://faucet.sanchonet.world.dev.cardano.org",
    apiKey: process.env.FAUCET_API_KEY || "",
  },
  kuber: {
    apiUrl:
      process.env.KUBER_API_URL || "https://sanchonet.kuber.cardanoapi.io",
    apiKey: process.env.KUBER_API_KEY || "",
  },
  txTimeOut: parseInt(process.env.TX_TIMEOUT) || 120000,
  metadataBucketUrl:
    process.env.METADATA_BUCKET_URL || "https://metadata.cardanoapi.io/data",
};

export default environments;
