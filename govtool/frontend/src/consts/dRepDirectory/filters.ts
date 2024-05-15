import { DRepStatus } from "@/models";

export const DREP_DIRECTORY_FILTERS = Object.values(DRepStatus)
  // `Yourself` should be excluded from filters
  .filter((status) => status !== DRepStatus.Yourself)
  .map((status) => ({
    key: status,
    label: status,
  }));
