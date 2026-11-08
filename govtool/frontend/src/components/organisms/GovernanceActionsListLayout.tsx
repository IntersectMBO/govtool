import { ReactNode, useState } from "react";
import { Box, ButtonBase, InputBase } from "@mui/material";
import { ListFilter, Search, X } from "lucide-react";

import { GovernanceActionTypeIcon } from "@atoms";
import { GOVERNANCE_ACTIONS_FILTERS } from "@consts";
import { useDataActionsBar, useFeatureFlag } from "@context";
import { useGetLiveProposalsQuery, useTranslation } from "@hooks";
import { FilterSidebar, SortSelect } from "@molecules";
import { ds, RADIUS } from "@/consts/designTokens";

type Props = { children: ReactNode };

export const SearchField = ({
  value,
  onChange,
  onClear,
  placeholder,
}: {
  value: string;
  onChange: (next: string) => void;
  /** Shows a clear button while there is text. */
  onClear?: () => void;
  placeholder: string;
}) => {
  const { t } = useTranslation();
  return (
    <Box
      sx={{
      alignItems: "center",
      backgroundColor: "#FFFFFF",
      border: `1px solid ${ds.gray["200"]}`,
      borderRadius: `${RADIUS.md}px`,
      display: "flex",
      flex: { xxs: "1 1 100%", md: 1 },
      gap: 1,
      height: 40,
      minWidth: 0,
      px: 1.5,
      "&:focus-within": { borderColor: ds.primary["600"] },
    }}
    >
      <Search aria-hidden color={ds.gray["500"]} size={16} />
      <InputBase
        inputProps={{ "aria-label": placeholder, "data-testid": "search-input" }}
        onChange={(event) => onChange(event.target.value)}
        placeholder={placeholder}
        sx={{ flex: 1, fontSize: 14, "& input": { p: 0 } }}
        value={value}
      />
      {onClear && value && (
      <ButtonBase
        aria-label={t("governanceActionHistoryList.clearSearchInput")}
        data-testid="clear-search-input"
        onClick={onClear}
        sx={{ borderRadius: "50%", color: ds.gray["500"], height: 24, width: 24 }}
      >
        <X aria-hidden size={14} />
      </ButtonBase>
    )}
    </Box>
  );
};

/** Counts per type from the cached live list, only when it holds every action. */
const useLiveTypeCounts = () => {
  const { liveProposals, liveProposalsTotal } = useGetLiveProposalsQuery();
  if (!liveProposals.length || liveProposals.length < liveProposalsTotal) return undefined;
  return liveProposals.reduce<Record<string, number>>((counts, proposal) => {
    counts[proposal.type] = (counts[proposal.type] ?? 0) + 1;
    return counts;
  }, {});
};

/**
 * The Live Voting frame: search and sort above, the action type filter in a
 * sidebar (behind a Filters button on small screens), the list beside it.
 */
export const GovernanceActionsListLayout = ({ children }: Props) => {
  const { t } = useTranslation();
  const {
    searchText,
    setSearchText,
    chosenFilters,
    setChosenFilters,
    chosenSorting,
    setChosenSorting,
  } = useDataActionsBar();
  const { governanceActionsSort } = useFeatureFlag();
  const counts = useLiveTypeCounts();
  const [areFiltersOpen, setAreFiltersOpen] = useState(false);

  const sidebar = (
    <FilterSidebar
      allCount={counts ? Object.values(counts).reduce((sum, n) => sum + n, 0) : undefined}
      allIcon={<ListFilter aria-hidden color={ds.primary["600"]} size={16} />}
      allLabel={t("listControls.allTypes")}
      dataTestId="action-type-filter"
      onChange={setChosenFilters}
      // The design lists Info Action first and No Confidence last.
      options={[...GOVERNANCE_ACTIONS_FILTERS].reverse().map((filter) => ({
        key: filter.key,
        label: filter.label,
        icon: <GovernanceActionTypeIcon type={filter.key} />,
        count: counts ? counts[filter.key] ?? 0 : undefined,
      }))}
      selected={chosenFilters}
      title={t("listControls.actionType")}
    />
  );

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
      <Box sx={{ alignItems: "center", display: "flex", flexWrap: { xxs: "wrap", md: "nowrap" }, gap: 1.5 }}>
        <SearchField
          onChange={setSearchText}
          placeholder={t("listControls.searchActions")}
          value={searchText}
        />
        <ButtonBase
          aria-expanded={areFiltersOpen}
          data-testid="filters-button"
          onClick={() => setAreFiltersOpen((open) => !open)}
          sx={{
            alignItems: "center",
            backgroundColor: "#FFFFFF",
            border: `1px solid ${chosenFilters.length ? ds.primary["600"] : ds.gray["200"]}`,
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
          {chosenFilters.length > 0 && ` (${chosenFilters.length})`}
        </ButtonBase>
        {governanceActionsSort.isAvailable && (
          <SortSelect
            onChange={setChosenSorting}
            options={governanceActionsSort.options}
            value={chosenSorting}
          />
        )}
      </Box>

      {areFiltersOpen && <Box sx={{ display: { md: "none" } }}>{sidebar}</Box>}

      <Box
        sx={{
          alignItems: "start",
          display: "grid",
          gap: 2.5,
          gridTemplateColumns: { xxs: "1fr", md: "230px minmax(0, 1fr)" },
        }}
      >
        <Box sx={{ display: { xxs: "none", md: "block" }, position: "sticky", top: 88 }}>
          {sidebar}
        </Box>
        <Box sx={{ minWidth: 0 }}>{children}</Box>
      </Box>
    </Box>
  );
};
