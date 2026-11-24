import { useEffect, useState } from "react";
import { Box, CircularProgress } from "@mui/material";

import { Typography } from "@atoms";
import { useCardano, useDataActionsBar } from "@context";
import { useGetProposalsPageQuery, useTranslation } from "@hooks";
import { SearchNotReady } from "@molecules";
import { ValidatedGovernanceActionCard } from "@organisms";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { ds } from "@/consts/designTokens";
import { clampPage } from "@/utils/clampPage";

const PAGE_SIZE = 10;

type GovernanceActionsToVoteProps = {
  onDashboard?: boolean;
};

/** The live governance actions as rows, one page at a time. */
export const GovernanceActionsToVote = ({
  onDashboard = true,
}: GovernanceActionsToVoteProps) => {
  const { pendingTransaction, isEnableLoading } = useCardano();
  const { debouncedSearchText, chosenSorting, chosenFilters } =
    useDataActionsBar();
  const { t } = useTranslation();
  const [page, setPage] = useState(1);

  useEffect(() => {
    setPage(1);
  }, [debouncedSearchText, chosenSorting, chosenFilters]);

  const {
    proposals,
    proposalsTotal,
    isProposalsLoading,
    isSearchNotReady,
  } = useGetProposalsPageQuery({
    filters: chosenFilters,
    page,
    pageSize: PAGE_SIZE,
    searchPhrase: debouncedSearchText,
    sorting: chosenSorting,
  });

  // An action can expire, or be voted on, while its last page is open; move
  // back to a page that still has rows rather than strand the user.
  useEffect(() => {
    if (!proposals) return;
    const lastPage = clampPage(page, proposalsTotal, PAGE_SIZE);
    if (lastPage !== page) setPage(lastPage);
  }, [proposals, proposalsTotal, page]);

  if (isSearchNotReady) return <SearchNotReady />;

  if (!proposals || isEnableLoading || isProposalsLoading) {
    return (
      <Box sx={{ display: "flex", justifyContent: "center", py: 6 }}>
        <CircularProgress />
      </Box>
    );
  }

  if (!proposals.length) {
    return (
      <Typography
        color={ds.gray["600"]}
        data-testid="governance-actions-empty"
        sx={{ py: 4 }}
        variant="paragraphSM"
      >
        {t("govActions.noResultsForTheSearch")}
      </Typography>
    );
  }

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      {proposals.map((item) => (
        <ValidatedGovernanceActionCard
          key={item.txHash + item.index}
          {...item}
          inProgress={
            onDashboard &&
            pendingTransaction.vote?.resourceId ===
              `${item.txHash ?? ""}${item.index ?? ""}`
          }
          txHash={item.txHash}
        />
      ))}
      <PaginationFooter
        onPageChange={(next) => {
          setPage(next);
          window.scrollTo({ top: 0, behavior: "smooth" });
        }}
        onPageSizeChange={() => {}}
        page={page}
        pageSize={PAGE_SIZE}
        total={proposalsTotal}
      />
    </Box>
  );
};
