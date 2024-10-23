import { bech32 } from "bech32";
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

// DBSync contains wrong representation of DRep view for script based DReps
export const fixViewForScriptBasedDRep = (view: string, isScriptBased: boolean) => {
  if (isScriptBased && !view.startsWith("drep_script")) {
    const { words } = bech32.decode(view);
    return bech32.encode("drep_script", words);
  }
  return view;
};
