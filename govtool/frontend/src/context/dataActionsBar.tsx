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
import { useLocation } from "react-router";

import { useDebounce } from "@hooks";
import { GOVERNANCE_ACTION_TYPE_PARAM, PATHS } from "@/consts/paths";

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

  // One type's list (/actions?type=…) counts as a page of its own.
  const { pathname: locationPathname, search } = useLocation();
  const type = new URLSearchParams(search).get(GOVERNANCE_ACTION_TYPE_PARAM);
  const pathname = type
    ? `${locationPathname}?${GOVERNANCE_ACTION_TYPE_PARAM}=${type}`
    : locationPathname;

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

  const gADetailsPathnameRegexp = /^\/actions\/gov_action1[0-9a-z]+$/;
  const typeListPath = `${PATHS.governanceActions}?${GOVERNANCE_ACTION_TYPE_PARAM}=`;
  const userMovedToDifferentAppArea =
    pathname !== lastPath &&
    (!pathname.startsWith(lastPath) || lastPath === "" || lastPath === "/");
  const userOpenedGADetails = gADetailsPathnameRegexp.test(pathname);
  const userOpenedGADetailsFromCategoryPage =
    userOpenedGADetails && lastPath.startsWith(typeListPath);
  const userMovedFromGAListToCategoryPage =
    lastPath === PATHS.governanceActions && pathname.startsWith(typeListPath);
  const userMovedFromGADetailsToListOrCategoryPage =
    (gADetailsPathnameRegexp.test(lastPath) &&
      pathname.startsWith(PATHS.governanceActions)) ||
    pathname.startsWith(typeListPath);
  const isSearchOrFilterSet =
    debouncedSearchText.length > 0 ||
    chosenSorting !== "" ||
    chosenFilters.length > 0;

  useEffect(() => {
    isAdjusting.current = true;
    if (userOpenedGADetails) {
      return;
    }

    if (userMovedFromGADetailsToListOrCategoryPage && isSearchOrFilterSet) {
      isAdjusting.current = false;
      return;
    }

    if (
      (!pathname.includes("/dreps") &&
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
