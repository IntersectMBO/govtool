import { Box, CircularProgress } from "@mui/material";
import { useSearchParams } from "react-router";

import { Button } from "@atoms";
import { GOV_ACTION_HISTORY_ITEMS_PER_PAGE } from "@consts";
import { useGetGovernanceActionHistoryQuery, useTranslation } from "@hooks";
import { GovernanceActionHistoryCard, GovernanceActionHistoryEmptyState } from "@molecules";

import "./governanceActionHistory.css";

export const GovernanceActionHistoryList = () => {
  const [searchParams] = useSearchParams();
  const { t } = useTranslation();

  const search = searchParams.get("q") || "";
  const filters = [
    ...(searchParams.get("type")?.split(",") ?? []),
    ...(searchParams.get("status")?.split(",") ?? []),
  ].filter(Boolean);
  const sort = searchParams.get("sort") || "";

  const {
    govActions,
    isGovActionsLoading,
    fetchNextPage,
    hasNextPage,
    isFetchingNextPage,
  } = useGetGovernanceActionHistoryQuery(
    search,
    filters,
    sort,
    GOV_ACTION_HISTORY_ITEMS_PER_PAGE,
  );

  const actions = govActions?.pages.flat() ?? [];

  return (
    <Box
      id="governance-actions-list-wrapper"
      data-testid="governance-actions-list-wrapper"
      component="section"
      display="flex"
      flexDirection="column"
      flexGrow={1}
      gap={2}
      width="100%"
    >
      {isGovActionsLoading && (
        <Box
          sx={{
            alignItems: "center",
            display: "flex",
            flex: 1,
            justifyContent: "center",
            minHeight: "75vh",
          }}
        >
          <CircularProgress />
        </Box>
      )}

      {!isGovActionsLoading && !actions.length && (
        <Box sx={{ paddingY: 3 }}>
          <GovernanceActionHistoryEmptyState
            title={t("governanceActionHistoryList.noResults.title")}
            description={t("governanceActionHistoryList.noResults.description")}
          />
        </Box>
      )}

      {actions.length > 0 && (
        <Box className="governance-actions-grid">
          {actions.map((action) => (
            <Box
              key={`${action.id}-${action.tx_hash}`}
              sx={{ display: "flex", minWidth: 0 }}
            >
              <GovernanceActionHistoryCard action={action} />
            </Box>
          ))}
        </Box>
      )}

      {hasNextPage && (
        <Box sx={{ justifyContent: "center", display: "flex" }}>
          <Button
            data-testid="show-more-button"
            variant="outlined"
            onClick={() => fetchNextPage()}
            isLoading={isFetchingNextPage}
          >
            {isFetchingNextPage
              ? t("actionRecord.loaders.loading")
              : t("governanceActionHistoryList.showMore")}
          </Button>
        </Box>
      )}
    </Box>
  );
};
