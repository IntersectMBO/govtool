import environments from "lib/constants/environments";
import fetch = require("node-fetch");

interface IFaucetResponse {
  amount: {
    lovelace: number;
  };
  txid: string;
  txin: string;
}

export const loadAmountFromFaucet = async (
  walletAddress: string,
): Promise<IFaucetResponse> => {
  try {
    const res = await fetchClient(
      `/send-money?type=default&action=funds&address=${walletAddress}&poolid=undefined&api_key=${environments.faucet.apiKey}`,
    );
    const responseBody = await res.json();
    console.debug(`faucet response: ${JSON.stringify(responseBody)}`);

    if (responseBody.error) {
      throw new Error("Error in loadAmountFaucet:" + responseBody.error.tag);
    }
    return responseBody;
  } catch (error) {
    console.error("Error in loadAmountFromFaucet:", error);
    throw error;
  }
};

const fetchClient = (url: string) => {
  return fetch(environments.faucet.apiUrl + url);
};
