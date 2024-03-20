import { Dispatch, FC, SetStateAction } from "react";
import { Box, InputBase } from "@mui/material";
import Search from "@mui/icons-material/Search";

import { GovernanceActionsFilters, GovernanceActionsSorting } from ".";
import { OrderActionsChip } from "./OrderActionsChip";
import { ClickOutside } from "../atoms";

import { theme } from "@/theme";

type DataActionsBarProps = {
  chosenFilters?: string[];
  chosenFiltersLength?: number;
  chosenSorting: string;
  closeFilters?: () => void;
  closeSorts: () => void;
  filtersOpen?: boolean;
  isFiltering?: boolean;
  searchText: string;
  setChosenFilters?: Dispatch<SetStateAction<string[]>>;
  setChosenSorting: Dispatch<SetStateAction<string>>;
  setFiltersOpen?: Dispatch<SetStateAction<boolean>>;
  setSearchText: Dispatch<SetStateAction<string>>;
  setSortOpen: Dispatch<SetStateAction<boolean>>;
  sortingActive: boolean;
  sortOpen: boolean;
};

export const DataActionsBar: FC<DataActionsBarProps> = ({ ...props }) => {
  const {
    chosenFilters = [],
    chosenFiltersLength,
    chosenSorting,
    closeFilters = () => {},
    closeSorts,
    filtersOpen,
    isFiltering = true,
    searchText,
    setChosenFilters = () => {},
    setChosenSorting,
    setFiltersOpen,
    setSearchText,
    setSortOpen,
    sortingActive,
    sortOpen,
  } = props;
  const {
    palette: { boxShadow2 },
  } = theme;

  return (
    <>
      <Box alignItems="center" display="flex" justifyContent="flex-start">
        <InputBase
          inputProps={{ "data-testid": "search-input" }}
          onChange={(e) => setSearchText(e.target.value)}
          placeholder="Search..."
          value={searchText}
          startAdornment={(
            <Search
              style={{
                color: "#99ADDE",
                height: 16,
                marginRight: 4,
                width: 16,
              }}
            />
          )}
          sx={{
            bgcolor: "white",
            border: 1,
            borderColor: "secondaryBlue",
            borderRadius: 50,
            boxShadow: `2px 2px 20px 0px ${boxShadow2}`,
            fontSize: 11,
            fontWeight: 500,
            height: 48,
            padding: "16px 24px",
            width: 231,
          }}
        />
        <OrderActionsChip
          chosenFiltersLength={chosenFiltersLength}
          filtersOpen={filtersOpen}
          isFiltering={isFiltering}
          setFiltersOpen={setFiltersOpen}
          setSortOpen={setSortOpen}
          sortingActive={sortingActive}
          sortOpen={sortOpen}
        />
      </Box>
      {filtersOpen && (
        <ClickOutside onClick={closeFilters}>
          <GovernanceActionsFilters
            chosenFilters={chosenFilters}
            setChosenFilters={setChosenFilters}
          />
        </ClickOutside>
      )}
      {sortOpen && (
        <ClickOutside onClick={closeSorts}>
          <GovernanceActionsSorting
            chosenSorting={chosenSorting}
            setChosenSorting={setChosenSorting}
          />
        </ClickOutside>
      )}
    </>
  );
};
