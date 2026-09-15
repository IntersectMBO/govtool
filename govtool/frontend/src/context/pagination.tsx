import React, {
  createContext,
  useContext,
  Dispatch,
  SetStateAction,
  FC,
  useState,
  useMemo,
} from "react";

type PaginationContextType = {
  page: number;
  pageSize: number;
  setPage: Dispatch<SetStateAction<number>>;
  setPageSize: Dispatch<SetStateAction<number>>;
};

const PaginationContext = createContext<
    PaginationContextType | undefined>(
  undefined);
PaginationContext.displayName = "PaginationContext";

type PaginationProviderProps = {
  children: React.ReactNode;
};

const PaginationProvider: FC<PaginationProviderProps> = ({ children }) => {
  const [page, setPage] = useState<number>(1);
  const [pageSize, setPageSize] = useState<number>(5);

  const contextValue = useMemo(
    () => ({
      page,
      pageSize,
      setPage,
      setPageSize,
    }),
    [page, pageSize],
  );

  return (
    <PaginationContext.Provider value={contextValue}>
      {children}
    </PaginationContext.Provider>
  );
};

function usePagination() {
  const ctx = useContext(PaginationContext);
  if (!ctx) {
    throw new Error("usePagination must be used within a PaginationProvider");
  }
  return ctx;
}

export { PaginationProvider, usePagination };
