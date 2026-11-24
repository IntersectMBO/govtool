import { Box, ButtonBase, MenuItem, Select, Typography } from "@mui/material";
import { FC } from "react";
import { useTranslation } from "react-i18next";

import { useScreenDimension } from "@/hooks/useScreenDimension";

import { ds, RADIUS } from "@/consts/designTokens";

type Props = {
  page: number;
  total: number;
  pageSize: number;
  onPageChange: (nextPage: number) => void;
  onPageSizeChange: (nextRpp: number) => void;
  pageSizeOptions?: number[];
  /** The redesign has no rows-per-page control; opt in to keep one. */
  showPageSize?: boolean;
};

type PageItem = number | "gap-start" | "gap-end";

// Every page up to 7; beyond that the first, the last and the current page
// with its neighbours, with an ellipsis for each skipped run.
const pageItems = (current: number, count: number): PageItem[] => {
  if (count <= 7) return Array.from({ length: count }, (_, i) => i + 1);
  const from = Math.max(2, current - 1);
  const to = Math.min(count - 1, current + 1);
  const middle = Array.from({ length: to - from + 1 }, (_, i) => from + i);
  return [
    1,
    ...(from > 2 ? (["gap-start"] as PageItem[]) : []),
    ...middle,
    ...(to < count - 1 ? (["gap-end"] as PageItem[]) : []),
    count,
  ];
};

const pageButtonSx = (isCurrent: boolean) => ({
  backgroundColor: isCurrent ? ds.primary["700"] : "#FFFFFF",
  border: isCurrent ? "none" : `1px solid ${ds.gray["200"]}`,
  borderRadius: `${RADIUS.md}px`,
  color: isCurrent ? "#FFFFFF" : ds.gray["500"],
  fontFamily: "inherit",
  fontSize: 14,
  fontWeight: 500,
  height: 36,
  letterSpacing: "-0.09px",
  lineHeight: "20px",
  minWidth: 36,
  px: 1.5,
  "&.Mui-disabled": { color: ds.gray["300"] },
});

const stepButtonSx = {
  ...pageButtonSx(false),
  flexShrink: 0,
  whiteSpace: "nowrap",
};

export const PaginationFooter: FC<Props> = ({
  page,
  total,
  pageSize,
  onPageChange,
  onPageSizeChange,
  pageSizeOptions = [5, 10, 25, 50],
  showPageSize = false,
}) => {
  const { t } = useTranslation();
  const { isMobile } = useScreenDimension();
  const pageCount = Math.max(1, Math.ceil((total || 0) / (pageSize || 1)));
  const clampedPage = Math.min(Math.max(page, 1), pageCount);

  const start = total === 0 ? 0 : (clampedPage - 1) * pageSize + 1;
  const end = total === 0 ? 0 : Math.min(clampedPage * pageSize, total);

  const handlePageSizeChange = (next: number) => {
    onPageSizeChange(next);
    const nextPageCount = Math.max(1, Math.ceil((total || 0) / next));
    if (clampedPage > nextPageCount) onPageChange(nextPageCount);
  };

  return (
    <Box
      aria-label={t("pagination.label")}
      component="nav"
      sx={{
        alignItems: "center",
        display: "flex",
        flexWrap: "wrap",
        gap: 2,
        justifyContent: "space-between",
        py: 1,
      }}
    >
      <Box sx={{ alignItems: "center", display: "flex", gap: 2 }}>
        <Typography
          sx={{
            color: ds.gray["600"],
            fontSize: 14,
            letterSpacing: "-0.09px",
            lineHeight: "20px",
          }}
        >
          {t("pagination.showing", { from: start, to: end, total })}
        </Typography>
        {showPageSize && (
          <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
            <Typography sx={{ color: ds.gray["600"], fontSize: 14, whiteSpace: "nowrap" }}>
              {t("pagination.rowsPerPage")}
            </Typography>
            <Select
              disableUnderline
              onChange={(e) => handlePageSizeChange(Number(e.target.value))}
              value={pageSize}
              variant="standard"
            >
              {pageSizeOptions.map((n) => (
                <MenuItem key={n} value={n}>
                  {n}
                </MenuItem>
              ))}
            </Select>
          </Box>
        )}
      </Box>

      <Box sx={{ alignItems: "center", display: "flex", gap: 0.75 }}>
        <ButtonBase
          disabled={clampedPage <= 1 || total === 0}
          onClick={() => onPageChange(Math.max(clampedPage - 1, 1))}
          sx={stepButtonSx}
        >
          <span aria-hidden>←&nbsp;</span>
          {t("pagination.previous")}
        </ButtonBase>
        {isMobile ? (
          <Typography
            sx={{
              color: ds.gray["600"],
              fontSize: 14,
              px: 1,
              whiteSpace: "nowrap",
            }}
          >
            {t("pagination.pageOf", { page: clampedPage, count: pageCount })}
          </Typography>
        ) : (
          pageItems(clampedPage, pageCount).map((item) =>
            (typeof item === "number" ? (
              <ButtonBase
                key={item}
                aria-current={item === clampedPage ? "page" : undefined}
                onClick={() => onPageChange(item)}
                sx={pageButtonSx(item === clampedPage)}
              >
                {item}
              </ButtonBase>
            ) : (
              <Typography
                key={item}
                sx={{ color: ds.gray["500"], fontSize: 14, px: 0.5 }}
              >
                …
              </Typography>
            )))
        )}
        <ButtonBase
          disabled={clampedPage >= pageCount || total === 0}
          onClick={() => onPageChange(Math.min(clampedPage + 1, pageCount))}
          sx={stepButtonSx}
        >
          {t("pagination.next")}
          <span aria-hidden>&nbsp;→</span>
        </ButtonBase>
      </Box>
    </Box>
  );
};
