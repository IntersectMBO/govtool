import { useState, useEffect } from "react";
import { Box, ButtonBase, CircularProgress } from "@mui/material";
import { useLocation } from "react-router";

import { PageContainer } from "@atoms";
import { useCardano, useDataActionsBar } from "@context";
import { useGetVoterInfo, useTranslation } from "@hooks";
import {
  GovernanceActionsToVote,
  DashboardGovernanceActionsVotedOn,
} from "@organisms";
import { ds, RADIUS } from "@/consts/designTokens";

import { GovernanceActionsHeader } from "./GovernanceActionsHeader";
import { GovernanceActionsListLayout } from "./GovernanceActionsListLayout";

type ListTabProps = {
  label: string;
  isActive: boolean;
  onClick: () => void;
  dataTestId: string;
};

const ListTab = ({ label, isActive, onClick, dataTestId }: ListTabProps) => (
  <ButtonBase
    aria-selected={isActive}
    data-testid={dataTestId}
    onClick={onClick}
    role="tab"
    sx={{
      backgroundColor: isActive ? "#FFFFFF" : "transparent",
      borderRadius: `${RADIUS.sm}px`,
      boxShadow: isActive ? "0px 1px 2px rgba(16, 24, 40, 0.08)" : "none",
      color: isActive ? ds.gray["900"] : ds.gray["500"],
      fontFamily: "inherit",
      fontSize: 14,
      fontWeight: isActive ? 600 : 500,
      height: 32,
      px: 1.5,
      "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` },
    }}
  >
    {label}
  </ButtonBase>
);

export const DashboardGovernanceActions = () => {
  const { debouncedSearchText } = useDataActionsBar();
  const { voter } = useGetVoterInfo();
  const { t } = useTranslation();
  const { isEnableLoading } = useCardano();
  const { state } = useLocation();

  const [content, setContent] = useState<number>(
    state?.isVotedListOnLoad ? 1 : 0,
  );

  useEffect(() => {
    window.history.replaceState({}, document.title);
  }, []);

  const isVoter = voter?.isRegisteredAsDRep || voter?.isRegisteredAsSoleVoter;

  return (
    <PageContainer sx={{ display: "flex", flex: 1, flexDirection: "column" }}>
      <GovernanceActionsHeader />
      <GovernanceActionsListLayout>
        {!voter || isEnableLoading ? (
          <Box sx={{ display: "flex", justifyContent: "center", py: 6 }}>
            <CircularProgress />
          </Box>
        ) : (
          <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
            {isVoter && (
              <Box
                aria-label={t("govActions.title")}
                role="tablist"
                sx={{
                  alignSelf: "flex-start",
                  backgroundColor: ds.gray["100"],
                  borderRadius: `${RADIUS.md}px`,
                  display: "flex",
                  gap: 0.5,
                  p: 0.5,
                }}
              >
                <ListTab
                  dataTestId="to-vote-tab"
                  isActive={content === 0}
                  label={t("govActions.toVote")}
                  onClick={() => setContent(0)}
                />
                <ListTab
                  dataTestId="voted-tab"
                  isActive={content === 1}
                  label={t("govActions.votedOnByMe")}
                  onClick={() => setContent(1)}
                />
              </Box>
            )}
            {content === 0 || !isVoter ? (
              <GovernanceActionsToVote onDashboard />
            ) : (
              <DashboardGovernanceActionsVotedOn
                searchPhrase={debouncedSearchText}
              />
            )}
          </Box>
        )}
      </GovernanceActionsListLayout>
    </PageContainer>
  );
};
