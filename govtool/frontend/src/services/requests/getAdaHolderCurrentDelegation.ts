import { CurrentDelegation } from "@models";

import { API } from "../API";
import { fixViewForScriptBasedDRep } from "@/utils";

export const getAdaHolderCurrentDelegation = async ({
  stakeKey,
}: {
  stakeKey?: string;
}) => {
  const response = await API.get<CurrentDelegation>(
    `/ada-holder/get-current-delegation/${stakeKey}`,
  );

  if (!response.data) return response.data;

  // DBSync contains wrong representation of DRep view for script based DReps
  const view = response.data.dRepView && fixViewForScriptBasedDRep(
    response.data.dRepView,
    response.data.isDRepScriptBased,
  );

  return {
    ...response.data,
    dRepView: view,
  };
};
