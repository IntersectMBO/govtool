/**
 * A maintenance notice from /maintenance.json, with its times already moved
 * into this browser's clock (milliseconds since the epoch).
 */
export type MaintenanceStatus = {
  startsAt?: number;
  endsAt?: number;
};
