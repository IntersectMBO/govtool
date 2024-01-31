import { API } from "../API";

export const postAdaHolderDelegateAbstain = async ({
  stakeKey,
}: {
  stakeKey: string;
}) => {
  const request = await API.post(`/ada-holder/delegate-to-abstain/${stakeKey}`);

  return request.data;
};
