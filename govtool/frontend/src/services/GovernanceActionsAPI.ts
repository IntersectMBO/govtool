import axios from "axios";

import { env } from "@/config/env";

// Use the same backend as every other GovTool request. Keep failures local to
// the requesting section instead of installing the global error-page redirect.
export const GovernanceActionsAPI = axios.create({
  baseURL: String(env.VITE_BASE_URL ?? "").replace(/\/+$/, ""),
  timeout: 30_000,
});
