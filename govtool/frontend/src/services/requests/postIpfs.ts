import { API } from "../API";
import { env } from "@/config/env";

export const postIpfs = async ({ content }: { content: string }) => {
  const headers: Record<string, string> = {
    "Content-Type": "text/plain;charset=utf-8",
  };

  if (env.VITE_IPFS_UPLOAD_API_KEY) {
    headers["Authorization"] = `Bearer ${env.VITE_IPFS_UPLOAD_API_KEY}`;
  }

  const response = await API.post("/ipfs/upload", content, {
    headers,
  });
  return response.data;
};
