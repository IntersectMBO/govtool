import axios from "axios";

export const getVoteContextTextFromFile = async (url: string | undefined) => {
  if (!url) {
    throw new Error("URL is undefined");
  }

  const response = await axios.get(url);

  return response.data.body?.body?.comment ?? "";
};
