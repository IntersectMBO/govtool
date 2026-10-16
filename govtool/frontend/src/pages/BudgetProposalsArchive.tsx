import React, { Suspense } from "react";
import { Box, CircularProgress } from "@mui/material";
import { Navigate, Route, Routes, useParams } from "react-router";
import { useCardano } from "@/context";
import { useScreenDimension, useTranslation } from "@/hooks";
import { useGetDRepVotingPowerList } from "@/hooks/queries";
import { Background, Typography } from "@/components/atoms";
import { Footer, TopNav } from "@/components/organisms";
import { BUDGET_DISCUSSION_PATHS } from "@/consts";
import type { BudgetArchiveProps } from "@/pdf-ui/BudgetArchiveApp";

const BudgetArchiveApp = React.lazy(() => import("@/pdf-ui/BudgetArchiveApp"));

// A full-width notice strip, in the style of GitHub's archived-repository
// banner.
const ArchiveBanner = () => {
  const { t } = useTranslation();
  return (
    <Box
      role="status"
      data-testid="budget-proposals-archive-banner"
      sx={{
        px: 2,
        py: 1.5,
        bgcolor: "#FFF8C5",
        borderTop: "1px solid rgba(212, 167, 44, 0.4)",
        borderBottom: "1px solid rgba(212, 167, 44, 0.4)",
        textAlign: "center",
      }}
    >
      <Typography
        variant="caption"
        component="p"
        sx={{ color: "#1F2328", fontSize: 14, fontWeight: 600 }}
      >
        {t("budgetProposalsArchive.bannerText")}
      </Typography>
    </Box>
  );
};

const ArchiveView = (
  props: Omit<BudgetArchiveProps, "fetchDRepVotingPowerList">,
) => {
  const { isEnabled } = useCardano();
  const { fetchDRepVotingPowerList } = useGetDRepVotingPowerList();
  return (
    <BudgetArchiveApp
      {...props}
      // The dashboard titles the page from the sidebar entry.
      showTitle={!isEnabled}
      fetchDRepVotingPowerList={fetchDRepVotingPowerList}
    />
  );
};

const DetailRoute = () => {
  const { id, proposalId } = useParams();
  return <ArchiveView view="detail" id={proposalId ?? id} />;
};

const CategoryRoute = () => {
  const { category } = useParams();
  return <ArchiveView view="list" category={category} />;
};

/**
 * The 2025 budget proposals, read-only, from static files. Served on the old
 * budget discussion paths so existing links resolve; it needs no backend and
 * is not behind the proposal discussion forum's feature flag.
 */
export const BudgetProposalsArchive = () => {
  const { isEnabled } = useCardano();
  const { pagePadding } = useScreenDimension();

  const content = (
    <Box
      sx={{
        display: "flex",
        flexDirection: "column",
        flex: 1,
        minHeight: !isEnabled ? "100vh" : "auto",
      }}
    >
      {!isEnabled && <TopNav />}
      <ArchiveBanner />
      <Box
        sx={{
          px: isEnabled ? { xxs: 2, md: 5 } : pagePadding,
          py: 3,
          display: "flex",
          flexDirection: "column",
          flex: 1,
        }}
      >
        <Suspense
          fallback={
            <Box
              sx={{
                display: "flex",
                flex: 1,
                alignItems: "center",
                justifyContent: "center",
              }}
            >
              <CircularProgress />
            </Box>
          }
        >
          <Routes>
            <Route index element={<ArchiveView view="list" />} />
            <Route
              path="propose"
              element={
                <Navigate
                  to={BUDGET_DISCUSSION_PATHS.budgetDiscussion}
                  replace
                />
              }
            />
            <Route path="category/:category" element={<CategoryRoute />} />
            <Route
              path="category/:category/:proposalId"
              element={<DetailRoute />}
            />
            <Route path=":id" element={<DetailRoute />} />
            <Route
              path="*"
              element={
                <Navigate
                  to={BUDGET_DISCUSSION_PATHS.budgetDiscussion}
                  replace
                />
              }
            />
          </Routes>
        </Suspense>
      </Box>
      {!isEnabled && <Footer />}
    </Box>
  );

  return isEnabled ? content : <Background>{content}</Background>;
};
