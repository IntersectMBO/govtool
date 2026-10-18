import axios from "axios";
import { env } from "@/config/env";
import I18n from "@/i18n";
import { getMaintenanceStatus } from "@/services/requests/getMaintenanceStatus";

type Options = {
  /** Refuse while the maintenance banner is up, as transactions must. */
  blockDuringNotice?: boolean;
};

/**
 * Runs before wallet actions. When the whole site is down for maintenance it
 * reloads onto nginx's maintenance page; with `blockDuringNotice` it also
 * refuses while maintenance.json announces maintenance (see nginx.conf).
 */
export const checkIsMaintenanceOn = async ({
  blockDuringNotice = false,
}: Options = {}) => {
  if (env.VITE_IS_DEV) return;

  try {
    const response = await axios.get(
      `${window.location.origin}/is-maintenance-mode-on`,
    );

    if (response.data === true || response.data === "true") {
      window.location.reload();
    }
  } catch {
    throw new Error("Action canceled due to maintenance mode.");
  }

  if (!blockDuringNotice) return;

  // Fails open: an unreadable notice must not block every transaction.
  const notice = await getMaintenanceStatus().catch(() => null);
  if (notice) {
    throw new Error(I18n.t("errors.maintenanceTransactionsPaused"));
  }
};
