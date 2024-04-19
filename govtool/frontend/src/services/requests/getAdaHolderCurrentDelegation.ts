import { CurrentDelegation } from "@models";

import { API } from "../API";

export const getAdaHolderCurrentDelegation = async ({
  stakeKey,
}: {
  stakeKey?: string;
}) => {
  const response = await API.get<CurrentDelegation>(
    `/ada-holder/get-current-delegation/${stakeKey}`,
  );

  return response.data;
};
