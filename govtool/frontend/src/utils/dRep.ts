import { DRepData } from "@/models";

export const isSameDRep = (
  { drepId, view }: DRepData,
  dRepIdOrView: string | undefined,
) => {
  if (!dRepIdOrView) {
    return false;
  }
  return drepId === dRepIdOrView || view === dRepIdOrView;
};
