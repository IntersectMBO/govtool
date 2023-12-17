import { API } from "../API";

export const getEpochParams = async () => {
  const response = await API.get("/epoch/params");

  return response.data;
};
