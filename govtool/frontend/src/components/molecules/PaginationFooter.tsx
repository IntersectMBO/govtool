import {
  Box,
  Typography,
  IconButton,
  Select,
  MenuItem,
  SelectChangeEvent,
} from "@mui/material";
import KeyboardArrowLeft from "@mui/icons-material/KeyboardArrowLeft";
import KeyboardArrowRight from "@mui/icons-material/KeyboardArrowRight";
import { FC } from "react";

type Props = {
  page: number;
  total: number;
  pageSize: number;
  onPageChange: (nextPage: number) => void;
  onPageSizeChange: (nextRpp: number) => void;
  pageSizeOptions?: number[];
};

export const PaginationFooter: FC<Props> = ({
  page,
  total,
  pageSize,
  onPageChange,
  onPageSizeChange,
  pageSizeOptions = [5, 10, 25, 50],
}) => {
  const pageCount = Math.max(1, Math.ceil((total || 0) / (pageSize || 1)));
  const clampedPage = Math.min(Math.max(page, 1), pageCount);

  const start = total === 0 ? 0 : (clampedPage - 1) * pageSize + 1;
  const end = total === 0 ? 0 : Math.min(clampedPage * pageSize, total);

  const handlePrev = () => onPageChange(Math.max(clampedPage - 1, 1));
  const handleNext = () => onPageChange(Math.min(clampedPage + 1, pageCount));

  const handlePageSizeChange = (e: SelectChangeEvent<number>) => {
    const next = Number(e.target.value);
    onPageSizeChange(next);

    const nextPageCount = Math.max(1, Math.ceil((total || 0) / next));
    if (clampedPage > nextPageCount) {
      onPageChange(nextPageCount);
    }
  };

  return (
    <Box
      sx={{
        display: "flex",
        alignItems: "center",
        justifyContent: "flex-end",
        gap: 2,
        px: 2,
        py: 1,
        color: "#3B485C",
      }}
    >
      <Box sx={{ display: "inline-flex", alignItems: "baseline", gap: 1.25 }}>
        <Typography variant="body2" sx={{ fontWeight: 500, color: "#3B485C" }}>
          Rows&nbsp;per&nbsp;page:
        </Typography>

        <Select
          value={pageSize}
          onChange={handlePageSizeChange}
          variant="standard"
          disableUnderline
          sx={{
            verticalAlign: "baseline",
            "& .MuiSelect-select": {
              py: 0,
              lineHeight: 1.5,
              display: "inline-flex",
              alignItems: "center",
            },
            "& .MuiSelect-icon": {
              top: "calc(50% - 12px)",
            },
          }}
          renderValue={(val) => (
            <Box
              component="span"
              sx={{ fontWeight: 500, color: "#3B485C", fontSize: 15 }}
            >
              {val as number}
            </Box>
          )}
          MenuProps={{
            PaperProps: { sx: { mt: 1, borderRadius: 1.5 } },
            MenuListProps: { dense: true },
          }}
        >
          {pageSizeOptions.map((n) => (
            <MenuItem key={n} value={n}>
              {n}
            </MenuItem>
          ))}
        </Select>
      </Box>

      <Typography variant="body2" sx={{ minWidth: 110, textAlign: "center" }}>
        {start}-{end} of {total}
      </Typography>

      <Box sx={{ display: "flex", alignItems: "center", gap: 0.5 }}>
        <IconButton
          size="small"
          onClick={handlePrev}
          disabled={clampedPage <= 1 || total === 0}
          aria-label="Previous page"
        >
          <KeyboardArrowLeft />
        </IconButton>

        <Typography
          variant="body2"
          sx={{ width: 24, textAlign: "center" }}
          aria-label="Current page"
        >
          {clampedPage}
        </Typography>

        <IconButton
          size="small"
          onClick={handleNext}
          disabled={clampedPage >= pageCount || total === 0}
          aria-label="Next page"
        >
          <KeyboardArrowRight />
        </IconButton>
      </Box>
    </Box>
  );
};
