import { LEGACY_PATH_SEGMENTS } from "@/consts/paths";

/**
 * The current path for a pre-rename URL such as /drep_directory/drep1...,
 * or null when no segment is a legacy one. Only whole segments are replaced,
 * so ids like gov_action1... are kept as they are.
 */
export const getCurrentPath = (pathname: string): string | null => {
  let changed = false;
  const path = pathname
    .split("/")
    .map((segment) => {
      if (
        !Object.prototype.hasOwnProperty.call(LEGACY_PATH_SEGMENTS, segment)
      ) {
        return segment;
      }
      changed = true;
      return LEGACY_PATH_SEGMENTS[segment];
    })
    .join("/");
  return changed ? path : null;
};
