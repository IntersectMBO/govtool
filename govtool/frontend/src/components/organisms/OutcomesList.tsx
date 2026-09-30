import { Box, CircularProgress } from "@mui/material";
import { useSearchParams } from "react-router";

import { Button } from "@atoms";
import { OUTCOMES_ITEMS_PER_PAGE } from "@consts";
import { useGetOutcomeGovernanceActionsQuery, useTranslation } from "@hooks";
import { OutcomeCard, OutcomesEmptyState } from "@molecules";

import "./outcomes.css";

export const OutcomesList = () => {
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
  } = useGetOutcomeGovernanceActionsQuery(
    search,
    filters,
    sort,
    OUTCOMES_ITEMS_PER_PAGE,
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
          <OutcomesEmptyState
            title={t("outcomesList.noResults.title")}
            description={t("outcomesList.noResults.description")}
          />
        </Box>
      )}

      {actions.length > 0 && (
        <Box className="outcomes-grid">
          {actions.map((action) => (
            <Box
              key={`${action.id}-${action.tx_hash}`}
              sx={{ display: "flex", minWidth: 0 }}
            >
              <OutcomeCard action={action} />
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
              ? t("outcome.loaders.loading")
              : t("outcomesList.showMore")}
          </Button>
        </Box>
      )}
    </Box>
  );
};
