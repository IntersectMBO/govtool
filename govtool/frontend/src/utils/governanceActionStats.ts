const WEEK = 7 * 24 * 60 * 60 * 1000;

type StatsAction = { status: { enacted_epoch: number | null } };
type StatsLive = { expiryDate?: string | null };

/**
 * The governance action header's four figures: every action on record, the
 * ones open for voting, those closing within 7 days, and the enacted ones.
 * A figure is undefined until its data arrives; Expiring Soon also while the
 * live list holds only a first page.
 */
export const governanceActionStats = ({
  actions,
  live,
  liveTotal,
  now,
}: {
  actions?: StatsAction[];
  live?: StatsLive[];
  liveTotal?: number;
  now: Date;
}) => {
  const at = now.getTime();
  const isLiveWhole = live !== undefined && liveTotal !== undefined && live.length >= liveTotal;
  return {
    total: actions?.length,
    votingNow: liveTotal,
    expiringSoon: isLiveWhole
      ? live.filter(({ expiryDate }) => {
        const time = expiryDate ? new Date(expiryDate).getTime() : NaN;
        return time >= at && time <= at + WEEK;
      }).length
      : undefined,
    enacted: actions?.filter(({ status }) => status.enacted_epoch !== null).length,
  };
};
