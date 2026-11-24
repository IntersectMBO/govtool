import {
  GitFork,
  HandCoins,
  Info,
  LucideIcon,
  ScrollText,
  ShieldX,
  SlidersHorizontal,
  Users,
} from "lucide-react";

import { ds } from "@/consts/designTokens";

const TYPE_ICONS: Record<string, { Icon: LucideIcon; color: string }> = {
  InfoAction: { Icon: Info, color: ds.primary["600"] },
  TreasuryWithdrawals: { Icon: HandCoins, color: "#E9682F" },
  ParameterChange: { Icon: SlidersHorizontal, color: ds.primary["600"] },
  NewConstitution: { Icon: ScrollText, color: ds.success["600"] },
  HardForkInitiation: { Icon: GitFork, color: ds.primary["600"] },
  NewCommittee: { Icon: Users, color: ds.success["600"] },
  NoConfidence: { Icon: ShieldX, color: ds.error["600"] },
};

type Props = { type: string; size?: number };

/** The redesign's icon for a governance action type; nothing for an unknown one. */
export const GovernanceActionTypeIcon = ({ type, size = 16 }: Props) => {
  const entry = TYPE_ICONS[type];
  if (!entry) return null;
  const { Icon, color } = entry;
  return <Icon aria-hidden color={color} size={size} strokeWidth={1.75} />;
};
