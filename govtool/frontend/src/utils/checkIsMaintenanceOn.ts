import axios from "axios";
import { env } from "@/config/env";

export const checkIsMaintenanceOn = async () => {
  if (env.VITE_IS_DEV) return;

  try {
    const response = await axios.get(
      `${window.location.protocol}//${window.location.hostname}/is-maintenance-mode-on`,
    );

    if (response.data) {
      window.location.reload();
    }
  } catch {
    throw new Error("Action canceled due to maintenance mode.");
  }
};
