import { format, isValid } from "date-fns";

/**
 * A date for display, or an empty string when there is none: a provider may
 * omit a timestamp it cannot know, and a missing date must not crash the page.
 */
export const formatDisplayDate = (
  date: string | Date | null | undefined,
  outputFormat = "do MMM yyyy",
) => {
  if (date === null || date === undefined || date === "") return "";
  const value = new Date(date);
  return isValid(value) ? format(value, outputFormat).toString() : "";
};
