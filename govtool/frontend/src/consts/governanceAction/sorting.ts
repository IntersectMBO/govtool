import type { GovActionSort } from "@/models/featureSet";

export const GOVERNANCE_ACTIONS_SORTING = [
  {
    key: "SoonestToExpire",
    label: "Soon to expire",
  },
  {
    key: "NewestCreated",
    label: "Newest first",
  },
  {
    key: "MostYesVotes",
    label: "Highest amount of 'Yes' votes",
  },
];

/**
 * UI sort key -> contract `GovActionSort` member. GovTool has no enum for these
 * three; they are the literals the list control sends to the backend.
 *
 * `NewestCreated` is the contract's `newest`; the contract also knows `oldest`
 * and `highestParticipation`, which this UI never offers.
 */
export const GOVERNANCE_ACTIONS_SORT_CAPABILITY_KEYS: Readonly<
  Record<string, GovActionSort>
> = {
  SoonestToExpire: "soonestToExpire",
  NewestCreated: "newest",
  MostYesVotes: "mostYesVotes",
};
