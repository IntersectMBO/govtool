import React, {
  createContext,
  useContext,
  useState,
  useCallback,
  Dispatch,
  SetStateAction,
  useEffect,
  useMemo,
  FC,
} from "react";
import { useLocation } from "react-router-dom";

import { useDebounce } from "@hooks";

interface DataActionsBarContextType {
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
  sortOpen: boolean;
}

const DataActionsBarContext = createContext<
  DataActionsBarContextType | undefined
>(undefined);
DataActionsBarContext.displayName = "DataActionsBarContext";

interface ProviderProps {
  children: React.ReactNode;
}

const DataActionsBarProvider: FC<ProviderProps> = ({ children }) => {
  const [searchText, setSearchText] = useState<string>("");
  const debouncedSearchText = useDebounce(searchText, 300);
  const [filtersOpen, setFiltersOpen] = useState<boolean>(false);
  const [chosenFilters, setChosenFilters] = useState<string[]>([]);
  const [sortOpen, setSortOpen] = useState<boolean>(false);
  const [chosenSorting, setChosenSorting] = useState<string>("");
  const [lastPath, setLastPath] = useState<string>("");

  const { pathname } = useLocation();

  const closeFilters = useCallback(() => {
    setFiltersOpen(false);
  }, []);

  const closeSorts = useCallback(() => {
    setSortOpen(false);
  }, []);

  const resetState = useCallback(() => {
    setSearchText("");
    setChosenFilters([]);
    setChosenSorting("");
  }, []);

  const userMovedToDifferentAppArea =
    pathname !== lastPath && !pathname.startsWith(lastPath);
  const userOpenedGADetailsFromCategoryPage =
    lastPath.includes("governance_actions/category") &&
    pathname.includes("governance_actions/");
  const userMovedFromGAListToCategoryPage =
    lastPath.endsWith("governance_actions") &&
    pathname.includes("governance_actions/category");

  useEffect(() => {
    if (
      (userMovedToDifferentAppArea && !userOpenedGADetailsFromCategoryPage) ||
      userMovedFromGAListToCategoryPage
    ) {
      resetState();
    }
  }, [pathname, resetState]);

  useEffect(() => {
    setLastPath(pathname);
  }, [searchText, chosenFilters, chosenSorting]);

  const contextValue = useMemo(
    () => ({
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
      sortOpen,
    }),
    [
      chosenFilters,
      chosenSorting,
      debouncedSearchText,
      filtersOpen,
      searchText,
      sortOpen,
      closeFilters,
      closeSorts,
    ],
  );

  return (
    <DataActionsBarContext.Provider value={contextValue}>
      {children}
    </DataActionsBarContext.Provider>
  );
};

function useDataActionsBar() {
  const context = useContext(DataActionsBarContext);
  if (!context) {
    throw new Error(
      "useDataActionsBar must be used within a DataActionsBarProvider",
    );
  }
  return context;
}

export { DataActionsBarProvider, useDataActionsBar };
