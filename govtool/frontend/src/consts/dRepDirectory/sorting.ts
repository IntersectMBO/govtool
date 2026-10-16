import type { DRepSort } from "@/models/featureSet";

import { DRepListSort } from "@/models";

export const DREP_DIRECTORY_SORTING = [
  {
    key: "Activity",
    label: "Voting Activity",
  },
  {
    key: "RegistrationDate",
    label: "Registration date",
  },
  {
    key: "VotingPower",
    label: "Voting power",
  },
  {
    key: "Status",
    label: "Status",
  },
  {
    key: "Random",
    label: "Random",
  },
];

/**
 * GovTool's sort control sends its own labels as wire values, so the capability
 * allow-list (which speaks the contract's `DRepSort` members) can only be
 * applied through an explicit translation.
 *
 * Typing the values as `DRepSort` is the point: a member renamed in the
 * contract fails this build instead of silently emptying the dropdown in the
 * browser.
 */
export const DREP_DIRECTORY_SORT_CAPABILITY_KEYS: Readonly<
  Record<string, DRepSort>
> = {
  [DRepListSort.Activity]: "activity",
  [DRepListSort.RegistrationDate]: "registrationDate",
  [DRepListSort.VotingPower]: "votingPower",
  [DRepListSort.Status]: "status",
  [DRepListSort.Random]: "random",
};
