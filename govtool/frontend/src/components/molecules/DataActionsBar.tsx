import { Dispatch, FC, SetStateAction, useMemo, useState } from "react";
import { Box, InputBase, IconButton } from "@mui/material";
import Search from "@mui/icons-material/Search";
import CloseIcon from "@mui/icons-material/Close";

import { DataActionsFilters, DataActionsSorting } from "@molecules";
import { OrderActionsChip } from "./OrderActionsChip";
import ChipButton from "../atoms/ChipButton";

type DataActionsBarProps = {
  chosenFilters?: string[];
  chosenFiltersLength?: number;
  chosenSorting: string;
  closeFilters?: () => void;
  closeSorts: () => void;
  filterOptions?: { key: string; label: string }[];
  filtersOpen?: boolean;
  filtersTitle?: string;
  isFiltering?: boolean;
  searchText: string;
  placeholder?: string;
  setChosenFilters?: Dispatch<SetStateAction<string[]>>;
  setChosenSorting: Dispatch<SetStateAction<string>>;
  setFiltersOpen?: Dispatch<SetStateAction<boolean>>;
  setSearchText: Dispatch<SetStateAction<string>>;
  setSortOpen?: Dispatch<SetStateAction<boolean>>;
  sortOpen?: boolean;
  sortOptions?: { key: string; label: string }[];
};

export const DataActionsBar: FC<DataActionsBarProps> = ({
  chosenFilters = [],
  chosenFiltersLength,
  chosenSorting,
  closeFilters = () => {},
  closeSorts,
  filterOptions = [],
  filtersOpen,
  filtersTitle,
  isFiltering = true,
  searchText,
  setChosenFilters = () => {},
  setChosenSorting,
  setFiltersOpen,
  setSearchText,
  setSortOpen,
  sortOpen,
  sortOptions = [],
  placeholder = "Search...",
}) => {
  const [localFiltersOpen, setLocalFiltersOpen] = useState(false);
  const [localSortOpen, setLocalSortOpen] = useState(false);

  const effectiveFiltersOpen = filtersOpen ?? localFiltersOpen;
  const effectiveSortOpen = sortOpen ?? localSortOpen;
  const setEffectiveFiltersOpen = setFiltersOpen ?? setLocalFiltersOpen;
  const setEffectiveSortOpen = setSortOpen ?? setLocalSortOpen;

  const selectedFilterItems = useMemo(
    () =>
      (chosenFilters ?? []).map((key) => ({
        key,
        label: (filterOptions ?? []).find((o) => o.key === key)?.label ?? key,
      })),
    [chosenFilters, filterOptions],
  );

  const handleRemoveFilter = (key: string) =>
    setChosenFilters?.((prev) => (prev ?? []).filter((k) => k !== key));

  return (
    <Box display="flex" flexDirection="column" gap={1.5}>
      <Box
        alignItems="center"
        display="flex"
        justifyContent="space-between"
        gap={{ xs: 0.75, sm: 1.5 }}
        flexWrap={{ xs: "wrap", sm: "nowrap" }}
        width="100%"
      >
        <InputBase
          inputProps={{ "data-testid": "search-input" }}
          onChange={(e) => setSearchText(e.target.value)}
          placeholder={placeholder}
          value={searchText}
          startAdornment={
            <Search
              style={{
                color: "#99ADDE",
                height: 16,
                marginRight: 4,
                width: 16,
              }}
            />
          }
          endAdornment={
            searchText ? (
              <IconButton
                size="small"
                onClick={() => setSearchText("")}
                sx={{ ml: 1 }}
              >
                <CloseIcon fontSize="small" />
              </IconButton>
            ) : null
          }
          sx={{
            bgcolor: "white",
            border: 1,
            borderColor: "#6E87D9",
            borderRadius: 50,
            boxShadow: "2px 2px 20px 0 rgba(0,0,0,0.05)",
            fontSize: 11,
            fontWeight: 500,
            height: 48,
            padding: "16px 24px",
            flex: "1 1 0",
            minWidth: 0,
            maxWidth: "100%",
          }}
        />

        <Box
          sx={{
            display: "flex",
            alignItems: "center",
            gap: { xs: 0.5, sm: 1.25 },
            flex: "0 0 auto",
            flexShrink: 0,
            mt: { xs: 1, sm: 0 },
          }}
        >
          <OrderActionsChip
            chosenFiltersLength={chosenFiltersLength}
            filtersOpen={effectiveFiltersOpen}
            isFiltering={isFiltering}
            setFiltersOpen={setEffectiveFiltersOpen}
            chosenSorting={chosenSorting}
            setSortOpen={setEffectiveSortOpen}
            sortOpen={effectiveSortOpen}
          >
            {effectiveFiltersOpen && (
              <DataActionsFilters
                chosenFilters={chosenFilters}
                setChosenFilters={setChosenFilters}
                closeFilters={closeFilters}
                options={filterOptions}
                title={filtersTitle}
              />
            )}
            {effectiveSortOpen && (
              <DataActionsSorting
                chosenSorting={chosenSorting}
                setChosenSorting={setChosenSorting}
                closeSorts={closeSorts}
                options={sortOptions}
              />
            )}
          </OrderActionsChip>
        </Box>
      </Box>

      {selectedFilterItems.length > 0 && (
        <Box
          display="flex"
          flexWrap="wrap"
          gap={1}
          alignItems="flex-start"
          sx={{ mt: 2, "& > *": { flex: "0 0 auto", alignSelf: "flex-start" } }}
        >
          {selectedFilterItems.map(({ key, label }) => (
            <ChipButton
              key={key}
              label={label}
              onDelete={() => handleRemoveFilter(key)}
              deleteIconPosition="left"
              bgColor="#B9CCF5"
              size="small"
            />
          ))}
        </Box>
      )}
    </Box>
  );
};

export default DataActionsBar;
