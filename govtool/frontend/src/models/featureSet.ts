/**
 * The feature set the backend serves at `/system/features`, and the two helpers
 * that read it. Kept here rather than imported from `@govtool/data-providers`:
 * CI builds the frontend from this directory alone, so it cannot depend on a
 * sibling package.
 */

/** A named UI surface the backend may declare unavailable. */
export type FeatureId = string;

/** A list control whose options the backend may narrow, such as a sort menu. */
export type ControlId = string;

export type FeatureSet = {
  provider?: string;
  network?: string;
  generatedAt?: string;
  /** Features that cannot be served, each with why. Absent means available. */
  unavailable: Record<FeatureId, { cause?: string; reason?: string }>;
  /** Allowed option values per control. A control not listed is not narrowed. */
  options: Record<ControlId, readonly string[]>;
  caveats?: unknown;
};

/** Sort keys the DRep directory may offer. */
export type DRepSort =
  | "votingPower"
  | "registrationDate"
  | "activity"
  | "status"
  | "random";

/** Sort keys the governance action list may offer. */
export type GovActionSort =
  | "newest"
  | "oldest"
  | "soonestToExpire"
  | "mostYesVotes"
  | "highestParticipation";

/** Fails open: with no feature set, or a feature not listed as unavailable, it is available. */
export const isAvailable = (
  featureSet: FeatureSet | undefined,
  feature: FeatureId,
): boolean => !featureSet?.unavailable || !(feature in featureSet.unavailable);

/**
 * The UI's options, in the UI's order, narrowed to what the control allows.
 * Fails open: with no feature set, or a control it does not narrow, every
 * option is allowed.
 */
export const allowedOptions = <V extends string>(
  featureSet: FeatureSet | undefined,
  control: ControlId,
  universe: readonly V[],
): readonly V[] => {
  const allowed = featureSet?.options?.[control];
  return allowed ? universe.filter((value) => allowed.includes(value)) : universe;
};
