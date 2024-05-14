import { DRepData } from "@/models";

export const isSameDRep = (
  { drepId, view }: DRepData,
  dRepIdOrView: string | undefined | null,
) => {
  if (!dRepIdOrView) {
    return false;
  }
  return drepId === dRepIdOrView || view === dRepIdOrView;
};
