import axios from "axios";

import { env } from "@/config/env";

const getOutcomesBaseURL = () => {
  const url = env.VITE_OUTCOMES_API_URL;
  if (url) return url.replace(/\/+$/, "");
  // Outcomes is served by GovTool's backend, including same-origin deployments.
  const backend = String(env.VITE_BASE_URL ?? "").replace(/\/+$/, "");
  return `${backend}/outcomes`;
};

// Separate from API: another base URL, and no redirect to the error page on
// a 500, so a failed outcomes call only empties the section that made it.
export const OutcomesAPI = axios.create({
  baseURL: getOutcomesBaseURL(),
});
