import { TransactionStatus } from "@/models";

import { API } from "../API";

export const getTransactionStatus = async (transactionHash: string) => {
  const response = await API.get<TransactionStatus>(
    `/transaction/status/${transactionHash}`,
  );

  return response.data;
};
