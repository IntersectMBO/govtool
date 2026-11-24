import { ReactNode, useEffect, useState } from "react";
import { Box, ButtonBase } from "@mui/material";
import { useSearchParams } from "react-router";
import { AlarmClock, Check, CircleCheck, ListFilter, Square } from "lucide-react";

import { GovernanceActionTypeIcon } from "@atoms";
import {
  GOV_ACTION_HISTORY_FILTERS_STORAGE_KEY,
  GOV_ACTION_HISTORY_SORT_OPTIONS,
  GOV_ACTION_HISTORY_SORT_STORAGE_KEY,
  GOV_ACTION_HISTORY_STATUS_FILTERS,
  GOV_ACTION_HISTORY_TYPE_FILTERS,
} from "@consts";
import { FilterSidebar, SearchField, SortSelect } from "@molecules";
import { useScreenDimension, useTranslation } from "@hooks";
import { getCurrentSearchParams } from "@utils";
import { useDRepDashboardActions } from "@/hooks/queries/useDRepDashboard";
import { ds, RADIUS } from "@/consts/designTokens";

// The Outcomes frame's status card: concluded statuses only, in its order and
// colours. "Live" stays a valid URL filter but is not offered here.
const STATUS_ROWS: { value: string; icon: ReactNode; color: string }[] = [
  { value: "expired", icon: <AlarmClock aria-hidden color={ds.error["600"]} size={16} />, color: ds.error["700"] },
  { value: "enacted", icon: <Check aria-hidden color={ds.success["600"]} size={16} />, color: ds.success["700"] },
  { value: "ratified", icon: <CircleCheck aria-hidden color={ds.success["600"]} size={16} />, color: ds.success["700"] },
];

// The frame's type card names and order.
const TYPE_ROWS = [
  "InfoAction",
  "TreasuryWithdrawals",
  "ParameterChange",
  "HardForkInitiation",
  "NewConstitution",
  "NewCommittee",
  "NoConfidence",
];

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
 * Search, filters and sort of the governanceActionHistory list, around the
 * list itself: search and sort above, type and status filters in a sidebar.
 * State lives in the URL (`q`, `type`, `status`, `sort`); filters and sort are
 * also remembered in localStorage and restored when the URL carries none.
 */
export const GovernanceActionHistorySearchFiltersSortBar = ({
  children,
}: {
  children?: ReactNode;
}) => {
  const { t } = useTranslation();
  const [areFiltersOpen, setAreFiltersOpen] = useState(false);
  // One set of filters in the DOM at a time, so their row ids stay unique.
  const { isMobile } = useScreenDimension();
  const [searchParams, setSearchParams] = useSearchParams();
  const [isInitializing, setIsInitializing] = useState(true);
  const [searchTerm, setSearchTerm] = useState(searchParams.get("q") || "");

  const typeParam = searchParams.get("type") ?? "";
  const statusParam = searchParams.get("status") ?? "";
  const currentSort = searchParams.get("sort") ?? "";
  const typeFilters = typeParam.split(",").filter(Boolean);
  const statusFilters = statusParam.split(",").filter(Boolean);
  // Every concluded action, cached with the governance action figures.
  const { actions } = useDRepDashboardActions();
  const typeCounts = actions?.reduce<Record<string, number>>((counts, action) => {
    counts[action.type] = (counts[action.type] ?? 0) + 1;
    return counts;
  }, {});
  const actionsCount = actions?.length ?? 0;

  useEffect(() => {
    const paramsToSet = new URLSearchParams(searchParams);

    if (!currentSort) {
      paramsToSet.set(
        "sort",
        readStorage(GOV_ACTION_HISTORY_SORT_STORAGE_KEY) ??
          GOV_ACTION_HISTORY_SORT_OPTIONS[0].value,
      );
    }

    if (!typeFilters.length && !statusFilters.length) {
      const savedFilters = readStorage(GOV_ACTION_HISTORY_FILTERS_STORAGE_KEY);
      if (savedFilters) {
        try {
          const parsed = JSON.parse(savedFilters);
          if (parsed.type?.length)
            paramsToSet.set("type", parsed.type.join(","));
          if (parsed.status?.length) {
            paramsToSet.set("status", parsed.status.join(","));
          }
        } catch {
          writeStorage(GOV_ACTION_HISTORY_FILTERS_STORAGE_KEY, null);
        }
      }
    }

    setSearchParams(paramsToSet, { replace: true });
    setIsInitializing(false);
  }, []);

  useEffect(() => {
    if (isInitializing) return;
    writeStorage(
      GOV_ACTION_HISTORY_FILTERS_STORAGE_KEY,
      typeParam || statusParam
        ? JSON.stringify({ type: typeFilters, status: statusFilters })
        : null,
    );
  }, [typeParam, statusParam, isInitializing]);

  useEffect(() => {
    if (isInitializing) return;
    writeStorage(GOV_ACTION_HISTORY_SORT_STORAGE_KEY, currentSort || null);
  }, [currentSort, isInitializing]);

  useEffect(() => {
    if (isInitializing) return;
    const timer = setTimeout(() => {
      if ((searchParams.get("q") ?? "") === searchTerm) return;
      const newParams = getCurrentSearchParams();
      if (searchTerm) {
        newParams.set("q", searchTerm);
      } else {
        newParams.delete("q");
      }
      setSearchParams(newParams);
    }, 300);
    return () => clearTimeout(timer);
  }, [searchTerm, searchParams, setSearchParams, isInitializing]);

  const setFilter = (name: FilterParam, values: string[]) => {
    const newParams = getCurrentSearchParams();
    if (values.length) {
      newParams.set(name, values.join(","));
    } else {
      newParams.delete(name);
    }
    setSearchParams(newParams);
  };

  const setSort = (value: string) => {
    const newParams = getCurrentSearchParams();
    newParams.set("sort", value);
    setSearchParams(newParams);
  };

  const sidebar = (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <FilterSidebar
        allIcon={<Square aria-hidden color={ds.gray["700"]} fill={ds.gray["700"]} size={10} />}
        allLabel={t("listControls.allStatuses")}
        checkboxes
        dataTestId="history-status-filter"
        onChange={(values) => setFilter("status", values)}
        options={STATUS_ROWS.map((row) => ({
          key: row.value,
          label:
            GOV_ACTION_HISTORY_STATUS_FILTERS.find((filter) => filter.value === row.value)
              ?.label ?? row.value,
          icon: row.icon,
          color: row.color,
        }))}
        selected={statusFilters}
      />
      <FilterSidebar
        allCount={typeCounts ? actionsCount : undefined}
        allIcon={<ListFilter aria-hidden color={ds.primary["600"]} size={16} />}
        allLabel={t("listControls.allTypes")}
        dataTestId="history-type-filter"
        onChange={(values) => setFilter("type", values)}
        options={TYPE_ROWS.filter((type) =>
          GOV_ACTION_HISTORY_TYPE_FILTERS.some((filter) => filter.value === type)).map((type) => ({
          key: type,
          label: t(`governanceActionHistoryList.filters.shortTypes.${type}`),
          // The ids keep the long names the E2E filter specs click.
          testLabel: t(`governanceActionHistoryList.filters.types.${type}`),
          icon: <GovernanceActionTypeIcon type={type} />,
          count: typeCounts ? typeCounts[type] ?? 0 : undefined,
        }))}
        selected={typeFilters}
      />
    </Box>
  );

  const chosenCount = typeFilters.length + statusFilters.length;

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box sx={{ alignItems: "center", display: "flex", flexWrap: { xxs: "wrap", md: "nowrap" }, gap: 1.5 }}>
        <SearchField
          onChange={setSearchTerm}
          onClear={() => setSearchTerm("")}
          placeholder={t("governanceActionHistoryList.searchPlaceholder")}
          value={searchTerm}
        />
        <ButtonBase
          aria-expanded={areFiltersOpen}
          data-testid="filters-button"
          onClick={() => setAreFiltersOpen((open) => !open)}
          sx={{
            alignItems: "center",
            backgroundColor: "#FFFFFF",
            border: `1px solid ${chosenCount ? ds.primary["600"] : ds.gray["200"]}`,
            borderRadius: `${RADIUS.md}px`,
            color: ds.gray["700"],
            display: { xxs: "inline-flex", md: "none" },
            flex: { xxs: 1, md: "none" },
            fontFamily: "inherit",
            fontSize: 14,
            gap: 0.75,
            height: 40,
            px: 1.5,
          }}
        >
          <ListFilter aria-hidden size={16} />
          {t("listControls.filters")}
          {chosenCount > 0 && ` (${chosenCount})`}
        </ButtonBase>
        <SortSelect
          onChange={setSort}
          options={GOV_ACTION_HISTORY_SORT_OPTIONS.map((option) => ({
            key: option.value,
            label: option.label,
          }))}
          value={currentSort}
        />
      </Box>

      {isMobile && areFiltersOpen && sidebar}

      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "1fr", md: "230px minmax(0, 1fr)" },
        }}
      >
        {!isMobile && (
          <Box sx={{ maxHeight: "calc(100vh - 104px)", overflowY: "auto", position: "sticky", top: 88 }}>
            {sidebar}
          </Box>
        )}
        <Box sx={{ minWidth: 0 }}>{children}</Box>
      </Box>
    </Box>
  );
};
