import axios from "axios";

import { env } from "@/config/env";

// Used when VITE_OUTCOMES_API_URL is unset, as the outcomes pillar package did.
const getDefaultOutcomesBaseURL = () => {
  const { hostname } = window.location;

  if (hostname === "localhost" || hostname === "127.0.0.1") {
    return "http://localhost:3000";
  }

  if (hostname.includes("preview.")) {
    return "https://outcomes-preview.1694.io/api";
  }

  return "https://outcomes.1694.io/api";
};

const getOutcomesBaseURL = () => {
  const url = env.VITE_OUTCOMES_API_URL;
  if (!url) return getDefaultOutcomesBaseURL();
  return url.endsWith("/") ? url.slice(0, -1) : url;
};

// Separate from API: another base URL, and no redirect to the error page on
// a 500, so a failed outcomes call only empties the section that made it.
export const OutcomesAPI = axios.create({
  baseURL: getOutcomesBaseURL(),
});
