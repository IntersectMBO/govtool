import { format } from "date-fns";

export const formatDisplayDate = (
  date: string | Date,
  outputFormat = "do MMM yyyy"
) => format(new Date(date), outputFormat).toString();
