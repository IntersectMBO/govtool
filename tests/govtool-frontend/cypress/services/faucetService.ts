import { IFaucetResponse } from "./types";

const faucetApiKey = Cypress.env("faucetApiKey");
const faucetApiUrl = Cypress.env("faucetApiUrl");

const fetchClient = (url: string) => {
  return fetch(faucetApiUrl + url);
};

export const loadAmountFromFaucet = async (
  walletAddress: string
): Promise<IFaucetResponse> => {
  try {
    const res = await fetchClient(
      `/send-money?type=default&action=funds&address=${walletAddress}&poolid=undefined&api_key=${faucetApiKey}`
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
