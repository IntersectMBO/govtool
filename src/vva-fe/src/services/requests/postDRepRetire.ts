import { API } from "../API";

export const postDRepRetire = async ({ drepId }: { drepId: string }) => {
  const request = await API.post("/drep/retire", {
    drepId,
  });
  return request.data;
};
