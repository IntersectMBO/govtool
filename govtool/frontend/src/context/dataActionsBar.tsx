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
  useRef,
} from "react";
import { useLocation } from "react-router-dom";

import { useDebounce } from "@hooks";

interface DataActionsBarContextType {
  isAdjusting: boolean;
  chosenFilters: string[];
  chosenFiltersLength: number;
  chosenSorting: string;
  closeFilters: () => void;
  closeSorts: () => void;
  debouncedSearchText: string;
  filtersOpen: boolean;
  searchText: string;
  lastPath: string;
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
  const isAdjusting = useRef<boolean>(false);
  const [searchText, setSearchText] = useState<string>("");
  const debouncedSearchText = useDebounce(searchText.trim(), 300);
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
    isAdjusting.current = false;
  }, []);

  const gADetailsPathnameRegexp = /^.*\/governance_actions\/[a-fA-F0-9]{64}$/;
  const userMovedToDifferentAppArea =
    pathname !== lastPath &&
    (!pathname.startsWith(lastPath) || lastPath === "" || lastPath === "/");
  const userOpenedGADetails = gADetailsPathnameRegexp.test(pathname);
  const userOpenedGADetailsFromCategoryPage =
    userOpenedGADetails &&
    lastPath.includes("governance_actions/category");
  const userMovedFromGAListToCategoryPage =
    lastPath.endsWith("governance_actions") &&
    pathname.includes("governance_actions/category");
  const userMovedFromGADetailsToListOrCategoryPage =
    (gADetailsPathnameRegexp.test(lastPath) &&
      pathname.includes("governance_actions")) ||
    pathname.includes("governance_actions/category");

  useEffect(() => {
    isAdjusting.current = true;
    if (userOpenedGADetails) {
      return;
    }

    if (userMovedFromGADetailsToListOrCategoryPage && debouncedSearchText.length > 0) {
      isAdjusting.current = false;
      return;
    }

    if (
      (!pathname.includes("drep_directory") &&
        userMovedToDifferentAppArea &&
        !userOpenedGADetailsFromCategoryPage) ||
      userMovedFromGAListToCategoryPage
    ) {
      resetState();
    }
  }, [pathname, resetState]);

  useEffect(() => {
    setLastPath(pathname);
  }, [pathname, searchText, chosenFilters, chosenSorting]);

  const contextValue = useMemo(
    () => ({
      isAdjusting: isAdjusting.current,
      chosenFilters,
      chosenFiltersLength: chosenFilters.length,
      chosenSorting,
      closeFilters,
      closeSorts,
      debouncedSearchText,
      filtersOpen,
      searchText,
      lastPath,
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
      pathname,
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
