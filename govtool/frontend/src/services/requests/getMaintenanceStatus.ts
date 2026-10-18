import axios from "axios";

import type { MaintenanceStatus } from "@/models";

/** A browser clock this far from the server's gets corrected. */
const CLOCK_SKEW_TOLERANCE_MS = 5_000;

const toTime = (value: unknown) => {
  if (typeof value !== "string") return undefined;
  const time = Date.parse(value);
  return Number.isNaN(time) ? undefined : time;
};

/**
 * The maintenance notice nginx serves from /var/run/maintenance.json (see
 * nginx.conf), or null when there is none. It comes from the page's own
 * origin, not the backend, so it answers while the API is down.
 *
 * Anything but a JSON object counts as no maintenance: a dev server answers
 * every path with index.html, and that must not raise the banner.
 */
export const getMaintenanceStatus =
  async (): Promise<MaintenanceStatus | null> => {
    const response = await axios.get<unknown>(
      `${window.location.origin}/maintenance.json`,
      { validateStatus: () => true },
    );

    const { data } = response;
    if (response.status !== 200 || typeof data !== "object" || !data) {
      return null;
    }

    // Count down on the server's clock, so a visitor whose clock is off by
    // minutes still sees the right time left. Small differences are ignored
    // to keep the countdown from jumping a second on each poll.
    const serverNow = Date.parse(String(response.headers.date ?? ""));
    const skew = Number.isNaN(serverNow) ? 0 : serverNow - Date.now();
    const offset = Math.abs(skew) > CLOCK_SKEW_TOLERANCE_MS ? skew : 0;

    const { startsAt, endsAt } = data as Record<string, unknown>;
    const toLocal = (time?: number) =>
      (time === undefined ? undefined : time - offset);

    return {
      startsAt: toLocal(toTime(startsAt)),
      endsAt: toLocal(toTime(endsAt)),
    };
  };
