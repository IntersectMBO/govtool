import { ReactNode } from "react";
import { useTranslation } from "react-i18next";
import { Box } from "@mui/material";
import { CircleCheck, Clock3, Layers, TriangleAlert } from "lucide-react";

import { StatCard } from "@/components/molecules/StatCard";
import { ds } from "@/consts/designTokens";
import { useDRepDashboardActions } from "@/hooks/queries/useDRepDashboard";
import { useGetLiveProposalsQuery } from "@/hooks/queries/useGetLiveProposalsQuery";
import { governanceActionStats } from "@/utils/governanceActionStats";

const Tile = ({ children, bg }: { children: ReactNode; bg: string }) => (
  <Box
    aria-hidden
    sx={{
      alignItems: "center",
      backgroundColor: bg,
      borderRadius: "50%",
      display: "flex",
      flexShrink: 0,
      height: 34,
      justifyContent: "center",
      width: 34,
    }}
  >
    {children}
  </Box>
);

/**
 * The governance action pages' four figures: every action on record, those
 * open for voting, those closing within 7 days and the enacted ones.
 */
export const GovernanceActionsStats = () => {
  const { t } = useTranslation();
  const { actions } = useDRepDashboardActions();
  const {
    liveProposals,
    liveProposalsTotal,
    areLiveProposalsLoading,
    isLiveProposalsError,
  } = useGetLiveProposalsQuery();
  // A list still loading, or one that failed, has no figures yet.
  const isLiveKnown = !areLiveProposalsLoading && !isLiveProposalsError;
  const stats = governanceActionStats({
    actions,
    live: isLiveKnown ? liveProposals : undefined,
    liveTotal: isLiveKnown ? liveProposalsTotal : undefined,
    now: new Date(),
  });

  const figure = (value: number | undefined) => {
    if (value !== undefined) return value.toLocaleString("en-US");
    return <span aria-label={t("govActions.stats.unknown")} role="img">—</span>;
  };

  const cards = [
    {
      id: "total",
      value: stats.total,
      color: ds.gray["900"],
      icon: <Tile bg={ds.gray["100"]}><Layers color={ds.gray["700"]} size={17} /></Tile>,
    },
    {
      id: "votingNow",
      value: stats.votingNow,
      color: ds.primary["700"],
      icon: <Tile bg={ds.primary["50"]}><Clock3 color={ds.primary["600"]} size={17} /></Tile>,
    },
    {
      id: "expiringSoon",
      value: stats.expiringSoon,
      color: ds.orange["700"],
      icon: <Tile bg={ds.warning["50"]}><TriangleAlert color={ds.warning["600"]} size={17} /></Tile>,
    },
    {
      id: "enacted",
      value: stats.enacted,
      color: ds.success["700"],
      icon: <Tile bg={ds.success["50"]}><CircleCheck color={ds.success["600"]} size={17} /></Tile>,
    },
  ] as const;

  return (
    <Box
      aria-label={t("govActions.stats.label")}
      component="section"
      sx={{
        display: "grid",
        gap: 2,
        gridTemplateColumns: { xxs: "repeat(2, minmax(0, 1fr))", md: "repeat(4, minmax(0, 1fr))" },
      }}
    >
      {cards.map((card) => (
        <StatCard
          key={card.id}
          dataTestId={`governance-actions-stat-${card.id}`}
          icon={card.icon}
          label={t(`govActions.stats.${card.id}`)}
          value={figure(card.value)}
          valueColor={card.color}
        />
      ))}
    </Box>
  );
};
