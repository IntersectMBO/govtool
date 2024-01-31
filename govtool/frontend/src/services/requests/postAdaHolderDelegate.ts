import { API } from "../API";

export const postAdaHolderDelegate = async ({
  dRepId,
  stakeKey,
}: {
  dRepId: string;
  stakeKey: string;
}) => {
  const request = await API.post("/ada-holder/delegate", {
    dRepId,
    stakeKey,
  });

  return request.data;
};
