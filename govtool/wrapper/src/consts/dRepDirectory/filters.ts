import { DRepStatus } from "@/models";

export const DREP_DIRECTORY_FILTERS = Object.values(DRepStatus).map((status) => ({
  key: status,
  label: status,
}));
