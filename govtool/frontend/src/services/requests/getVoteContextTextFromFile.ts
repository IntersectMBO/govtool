import axios from "axios";

export const getVoteContextTextFromFile = async (url: string | undefined) => {
  if (!url) {
    throw new Error("URL is undefined");
  }

  const response = await axios.get(url);

  const voteContextText =
    response.data.body["CIP108:voteContextText"]["@value"];

  return voteContextText;
};
