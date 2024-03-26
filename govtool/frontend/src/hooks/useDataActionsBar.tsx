import { useState, useCallback, Dispatch, SetStateAction } from "react";

import {
  useDebounce,
} from "@hooks";

type UseDataActionsBarReturnType = {
  chosenFilters: string[];
  chosenFiltersLength: number;
  chosenSorting: string;
  closeFilters: () => void;
  closeSorts: () => void;
  debouncedSearchText: string;
  filtersOpen: boolean;
  searchText: string;
  setChosenFilters: Dispatch<SetStateAction<string[]>>;
  setChosenSorting: Dispatch<SetStateAction<string>>;
  setFiltersOpen: Dispatch<SetStateAction<boolean>>;
  setSearchText: Dispatch<SetStateAction<string>>;
  setSortOpen: Dispatch<SetStateAction<boolean>>;
  sortingActive: boolean;
  sortOpen: boolean;
};

export const useDataActionsBar = (): UseDataActionsBarReturnType => {
  const [searchText, setSearchText] = useState<string>("");
  const debouncedSearchText = useDebounce(searchText, 300);
  const [filtersOpen, setFiltersOpen] = useState(false);
  const [chosenFilters, setChosenFilters] = useState<string[]>([]);
  const [sortOpen, setSortOpen] = useState(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");

  const closeFilters = useCallback(() => {
    setFiltersOpen(false);
  }, [setFiltersOpen]);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, [setSortOpen]);

  return {
    chosenFilters,
    chosenFiltersLength: chosenFilters.length,
    chosenSorting,
    closeFilters,
    closeSorts,
    debouncedSearchText,
    filtersOpen,
    searchText,
    setChosenFilters,
    setChosenSorting,
    setFiltersOpen,
    setSearchText,
    setSortOpen,
    sortingActive: Boolean(chosenSorting),
    sortOpen,
  };
};
