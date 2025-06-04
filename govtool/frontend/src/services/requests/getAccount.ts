import { Account } from "@/models";
import { API } from "../API";

export const getAccount = async ({ stakeKey }: { stakeKey?: string }) => {
  const response = await API.get<Account>(`/account/${stakeKey || ""}`);

  return response.data;
};
