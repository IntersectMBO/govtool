import { API } from "../API";

export const getTransactionStatus = async (transactionHash: string) => {
  const response = await API.get(`/transaction/status/${transactionHash}`);

  return response.data;
};
