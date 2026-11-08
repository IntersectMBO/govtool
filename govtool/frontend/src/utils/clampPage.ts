/** The page to show once `total` items remain: never past the last page, never below 1. */
export const clampPage = (page: number, total: number, pageSize: number) =>
  Math.min(Math.max(page, 1), Math.max(1, Math.ceil(total / pageSize)));
