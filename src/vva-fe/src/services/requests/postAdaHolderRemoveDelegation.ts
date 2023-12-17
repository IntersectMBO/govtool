import { API } from "../API";

export const postAdaHolderRemoveDelegation = async ({
  stakeKey,
}: {
  stakeKey: string;
}) => {
  const request = await API.post("/ada-holder/remove-delegation", {
    stakeKey,
  });

  return request.data;
};
