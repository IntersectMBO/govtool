import { API } from "../API";

export const postAdaHolderDelegateNo = async ({
  stakeKey,
}: {
  stakeKey: string;
}) => {
  const request = await API.post(`/ada-holder/delegate-to-no-confidence/${stakeKey}`);

  return request.data;
};
