import type { DRepVotingPowerListResponse } from "@/models";

export type BudgetArchiveProps = {
  view: "list" | "detail";
  /** The master id, for the detail view. */
  id?: string;
  /** A category slug, for /budget_discussion/category/:category. */
  category?: string;
  /** False where the host already titles the page (the dashboard). */
  showTitle?: boolean;
  /** Names the DReps among the commenters. */
  fetchDRepVotingPowerList?: (
    identifiers: string[],
  ) => Promise<DRepVotingPowerListResponse>;
};

export default function BudgetArchiveApp(
  props: BudgetArchiveProps,
): JSX.Element;
