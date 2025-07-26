import { API } from "../API";

export const postIpfs = async ({ content }: { content: string }) => {
  const response = await API.post("/ipfs/upload", content,{
    headers: {
      "Content-Type": "text/plain;charset=utf-8"
    }
  });
  return response.data;
};
