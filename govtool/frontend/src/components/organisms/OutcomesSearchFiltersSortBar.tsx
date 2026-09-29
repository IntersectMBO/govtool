import { useEffect, useState } from "react";
import { Box, Chip, IconButton, InputBase } from "@mui/material";
import { useSearchParams } from "react-router";
import { IconSearch, IconX } from "@intersect.mbo/intersectmbo.org-icons-set";

import {
  orange,
  OUTCOMES_FILTERS_STORAGE_KEY,
  OUTCOMES_SORT_OPTIONS,
  OUTCOMES_SORT_STORAGE_KEY,
  OUTCOMES_STATUS_FILTERS,
  OUTCOMES_TYPE_FILTERS,
  primaryBlue,
} from "@consts";
import { OutcomesFilters, OutcomesSorting } from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";

type FilterParam = "type" | "status";

const readStorage = (key: string) => {
  try {
    return window.localStorage.getItem(key);
  } catch {
    return null;
  }
};

const writeStorage = (key: string, value: string | null) => {
  try {
    if (value === null) {
      window.localStorage.removeItem(key);
    } else {
      window.localStorage.setItem(key, value);
    }
  } catch {
    // Storage can be unavailable (private mode); the URL still holds state.
  }
};

/**
 * Search, filters and sort of the outcomes list. State lives in the URL
 * (`q`, `type`, `status`, `sort`); filters and sort are also remembered in
 * localStorage and restored when the URL carries none.
 */
export const OutcomesSearchFiltersSortBar = () => {
  const { isMobile } = useScreenDimension();
  const { t } = useTranslation();
  const [searchParams, setSearchParams] = useSearchParams();
  const [isInitializing, setIsInitializing] = useState(true);
  const [searchTerm, setSearchTerm] = useState(searchParams.get("q") || "");

  const typeParam = searchParams.get("type") ?? "";
  const statusParam = searchParams.get("status") ?? "";
  const currentSort = searchParams.get("sort") ?? "";
  const typeFilters = typeParam.split(",").filter(Boolean);
  const statusFilters = statusParam.split(",").filter(Boolean);

  useEffect(() => {
    const paramsToSet = new URLSearchParams(searchParams);

    if (!currentSort) {
      paramsToSet.set(
        "sort",
        readStorage(OUTCOMES_SORT_STORAGE_KEY) ??
          OUTCOMES_SORT_OPTIONS[0].value,
      );
    }

    if (!typeFilters.length && !statusFilters.length) {
      const savedFilters = readStorage(OUTCOMES_FILTERS_STORAGE_KEY);
      if (savedFilters) {
        try {
          const parsed = JSON.parse(savedFilters);
          if (parsed.type?.length)
            paramsToSet.set("type", parsed.type.join(","));
          if (parsed.status?.length) {
            paramsToSet.set("status", parsed.status.join(","));
          }
        } catch {
          writeStorage(OUTCOMES_FILTERS_STORAGE_KEY, null);
        }
      }
    }

    setSearchParams(paramsToSet, { replace: true });
    setIsInitializing(false);
  }, []);

  useEffect(() => {
    if (isInitializing) return;
    writeStorage(
      OUTCOMES_FILTERS_STORAGE_KEY,
      typeParam || statusParam
        ? JSON.stringify({ type: typeFilters, status: statusFilters })
        : null,
    );
  }, [typeParam, statusParam, isInitializing]);

  useEffect(() => {
    if (isInitializing) return;
    writeStorage(OUTCOMES_SORT_STORAGE_KEY, currentSort || null);
  }, [currentSort, isInitializing]);

  useEffect(() => {
    if (isInitializing) return;
    const timer = setTimeout(() => {
      if ((searchParams.get("q") ?? "") === searchTerm) return;
      const newParams = new URLSearchParams(searchParams);
      if (searchTerm) {
        newParams.set("q", searchTerm);
      } else {
        newParams.delete("q");
      }
      setSearchParams(newParams);
    }, 300);
    return () => clearTimeout(timer);
  }, [searchTerm, searchParams, setSearchParams, isInitializing]);

  const removeFilter = (name: FilterParam, value: string) => {
    const newParams = new URLSearchParams(searchParams);
    const updated = (newParams.get(name) ?? "")
      .split(",")
      .filter((item) => item && item !== value);
    if (updated.length) {
      newParams.set(name, updated.join(","));
    } else {
      newParams.delete(name);
    }
    setSearchParams(newParams);
  };

  const getFilterLabel = (name: FilterParam, value: string) =>
    (name === "type" ? OUTCOMES_TYPE_FILTERS : OUTCOMES_STATUS_FILTERS).find(
      (filter) => filter.value === value,
    )?.label ?? value;

  const renderChip = (name: FilterParam, value: string) => (
    <Chip
      key={`${name}-${value}`}
      label={getFilterLabel(name, value)}
      onDelete={() => removeFilter(name, value)}
      data-testid={`${name}-chip-${value}`}
      size="small"
      deleteIcon={
        <IconButton sx={{ padding: "4px" }}>
          <IconX style={{ width: 16, height: 16 }} />
        </IconButton>
      }
      sx={{
        backgroundColor: name === "type" ? "#B8CDFF" : orange.c200,
        borderRadius: 100,
        flexDirection: "row-reverse",
        gap: 0.5,
        height: "auto",
        py: 0.75,
        pl: 1.75,
        pr: 2.25,
        "& .MuiChip-label": {
          color: "textBlack",
          fontSize: 12,
          fontWeight: 400,
          px: 0,
        },
      }}
    />
  );

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 3 }}>
      <Box
        display="flex"
        flexDirection={isMobile ? "column" : "row"}
        alignItems={isMobile ? "stretch" : "center"}
        justifyContent="space-between"
        gap={isMobile ? 1 : 1.5}
      >
        <InputBase
          id="search-input"
          inputProps={{ "data-testid": "search-input" }}
          onChange={(e) => setSearchTerm(e.target.value)}
          placeholder={t("outcomesList.searchPlaceholder")}
          value={searchTerm}
          startAdornment={
            <IconSearch width={20} height={18} fill={primaryBlue.c200} />
          }
          endAdornment={
            searchTerm && (
              <IconButton
                onClick={() => setSearchTerm("")}
                data-testid="clear-search-input"
                aria-label={t("outcomesList.clearSearchInput")}
              >
                <IconX width={18} height={18} />
              </IconButton>
            )
          }
          sx={{
            bgcolor: "white",
            border: 1,
            borderColor: "#BFC8D961",
            borderRadius: 1,
            fontSize: 15,
            fontWeight: 400,
            height: 48,
            paddingLeft: 2.5,
            "& .MuiInputBase-input": { paddingLeft: 1 },
          }}
        />
        <Box display="flex" gap={isMobile ? 1 : 1.5}>
          <OutcomesFilters />
          <OutcomesSorting />
        </Box>
      </Box>

      {(typeFilters.length > 0 || statusFilters.length > 0) && (
        <Box display="flex" flexWrap="wrap" gap={1} data-testid="filter-chips">
          {typeFilters.map((value) => renderChip("type", value))}
          {statusFilters.map((value) => renderChip("status", value))}
        </Box>
      )}
    </Box>
  );
};
