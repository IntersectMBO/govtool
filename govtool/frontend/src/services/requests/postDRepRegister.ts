import { API } from "../API";

export const postDRepRegister = async ({
  drepId,
  url,
  metadataHash,
}: {
  drepId: string;
  url: string;
  metadataHash: string;
}) => {
  const request = await API.post("/drep/register", {
    drepId,
    url,
    metadataHash,
  });
  return request.data;
};
