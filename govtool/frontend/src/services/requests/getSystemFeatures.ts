import type { FeatureSet } from "@/models/featureSet";

import { API } from "../API";

/**
 * The feature set the backend composes from the active provider's declaration
 * and its own — a provider that honours no DRep sort key does NOT disable the
 * UI sort, because GovTool's backend sorts the directory in memory.
 *
 * Typed by `@/models/featureSet`, the wire form of the backend's feature set.
 */
export const getSystemFeatures = async (): Promise<FeatureSet> => {
  // `validateStatus` is load-bearing, not defensive noise: the shared API
  // instance has a response interceptor that NAVIGATES to the error page on any
  // 500. Capabilities are advisory — a backend that cannot derive them must
  // leave the app on the page the user is looking at — so this one call resolves
  // on every status and decides for itself.
  const response = await API.get<FeatureSet>("/system/features", {
    validateStatus: () => true,
  });

  if (response.status !== 200) {
    throw new Error(`/system/features responded ${response.status}`);
  }

  // A backend older than the capability layer may answer this path with
  // something else entirely. Refusing an unrecognised document is what keeps
  // the gates failing OPEN (full UI) instead of half-gating on a partial shape.
  if (
    typeof response.data?.unavailable !== "object" ||
    typeof response.data?.options !== "object"
  ) {
    throw new Error("/system/features did not answer with a feature set");
  }

  return response.data;
};
