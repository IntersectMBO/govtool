import { Dispatch, KeyboardEvent, SetStateAction, useRef, useState } from "react";
import { Box } from "@mui/material";
import { useTranslation } from "react-i18next";
import {
  Ban,
  GitBranch,
  Info,
  Search,
  Settings,
  Shield,
  Users,
  Wallet,
  type LucideIcon,
} from "lucide-react";

import { CenteredBoxBottomButtons } from "@/components/molecules/CenteredBoxBottomButtons";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";
import { useCreateGovernanceActionForm } from "@/hooks/forms/useCreateGovernanceActionForm";
import { GovernanceActionType } from "@/types/governanceAction";

import { SectionCard, StepHeading } from "./parts";

type ChooseGovernanceActionTypeProps = {
  setStep: Dispatch<SetStateAction<number>>;
};

/** The order and icons the design gives the action types. */
const TYPES: { type: GovernanceActionType; icon: LucideIcon }[] = [
  { type: GovernanceActionType.TreasuryWithdrawals, icon: Wallet },
  { type: GovernanceActionType.InfoAction, icon: Info },
  { type: GovernanceActionType.HardForkInitiation, icon: GitBranch },
  { type: GovernanceActionType.NewConstitution, icon: Shield },
  { type: GovernanceActionType.NoConfidence, icon: Ban },
  { type: GovernanceActionType.ParameterChange, icon: Settings },
  { type: GovernanceActionType.NewCommittee, icon: Users },
];

export const ChooseGovernanceActionType = ({
  setStep,
}: ChooseGovernanceActionTypeProps) => {
  const { t } = useTranslation();
  const { setValue, watch } = useCreateGovernanceActionForm();
  const [search, setSearch] = useState("");
  const options = useRef<(HTMLDivElement | null)[]>([]);
  const selected = watch("governance_action_type");

  const term = search.trim().toLowerCase();
  const visible = TYPES.filter(({ type }) =>
    !term ||
    t(`createGA.type.names.${type}`).toLowerCase().includes(term) ||
    t(`createGA.type.descriptions.${type}`).toLowerCase().includes(term));

  // The roving tab stop: the selected card, or the first one shown when the
  // search hides it, so Tab always reaches the group.
  const tabStop = Math.max(0, visible.findIndex(({ type }) => type === selected));

  const choose = (type: GovernanceActionType) => setValue("governance_action_type", type);

  const onKeyDown = (event: KeyboardEvent, index: number) => {
    const moves: Record<string, number> = {
      ArrowDown: 1,
      ArrowRight: 1,
      ArrowUp: -1,
      ArrowLeft: -1,
    };
    if (event.key in moves) {
      event.preventDefault();
      const next = (index + moves[event.key] + visible.length) % visible.length;
      choose(visible[next].type);
      options.current[next]?.focus();
    } else if (event.key === " " || event.key === "Enter") {
      event.preventDefault();
      choose(visible[index].type);
    }
  };

  return (
    <>
      <StepHeading body={t("createGA.type.body")} title={t("createGA.type.title")} />
      <SectionCard testId="create-ga-types">
        <Box sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", gap: 1, height: 42, mb: 1.5, px: 1.5, "&:focus-within": { borderColor: ds.primary["600"] } }}>
          <Search aria-hidden color={ds.gray["500"]} size={16} />
          <Box
            aria-label={t("createGA.type.search")}
            component="input"
            onChange={(event) => setSearch((event.target as HTMLInputElement).value)}
            placeholder={t("createGA.type.search")}
            sx={{ border: "none", color: ds.gray["900"], flex: 1, fontSize: 14, outline: "none" }}
            type="search"
            value={search}
          />
        </Box>
        <Box aria-label={t("createGA.type.title")} role="radiogroup" sx={{ display: "flex", flexDirection: "column", gap: 1.5, maxHeight: 520, overflowY: "auto", p: 0.25 }}>
          {!visible.length && <Box sx={{ color: ds.gray["600"], fontSize: 14, py: 3, textAlign: "center" }}>{t("createGA.type.noMatch")}</Box>}
          {visible.map(({ type, icon: Icon }, index) => {
            const isSelected = selected === type;
            return (
              <Box
                key={type}
                ref={(node: HTMLDivElement | null) => { options.current[index] = node; }}
                aria-checked={isSelected}
                data-testid={`${type}-radio`}
                onClick={() => choose(type)}
                onKeyDown={(event) => onKeyDown(event, index)}
                role="radio"
                sx={{
                  backgroundColor: "#FFFFFF",
                  border: `${isSelected ? 2 : 1}px solid ${isSelected ? ds.primary["600"] : ds.gray["200"]}`,
                  borderRadius: `${RADIUS.lg}px`,
                  cursor: "pointer",
                  p: isSelected ? "15px" : 2,
                  "&:hover": { borderColor: ds.primary["600"] },
                  "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
                }}
                tabIndex={index === tabStop ? 0 : -1}
              >
                <Box sx={{ alignItems: "center", display: "flex", gap: 1.25 }}>
                  <Box aria-hidden sx={{ alignItems: "center", backgroundColor: isSelected ? ds.primary["600"] : ds.gray["100"], borderRadius: "50%", color: isSelected ? "#FFFFFF" : ds.gray["700"], display: "flex", height: 26, justifyContent: "center", width: 26 }}>
                    <Icon size={14} />
                  </Box>
                  <Box sx={{ color: ds.gray["900"], flex: 1, fontSize: 16, fontWeight: 600 }}>{t(`createGA.type.names.${type}`)}</Box>
                  <Box
                    component="span"
                    sx={{
                      backgroundColor: isSelected ? ds.primary["600"] : PILL_TONES.neutral.bg,
                      borderRadius: `${RADIUS.pill}px`,
                      color: isSelected ? "#FFFFFF" : PILL_TONES.neutral.text,
                      fontSize: 12,
                      fontWeight: 500,
                      px: 1.25,
                      py: 0.25,
                    }}
                  >
                    {isSelected ? t("createGA.type.selected") : t("createGA.type.available")}
                  </Box>
                </Box>
                <Box sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", mt: 1 }}>{t(`createGA.type.descriptions.${type}`)}</Box>
              </Box>
            );
          })}
        </Box>
      </SectionCard>
      <CenteredBoxBottomButtons
        backButtonText={t("cancel")}
        disableActionButton={!selected}
        onActionButton={() => setStep(3)}
        onBackButton={() => setStep(1)}
      />
    </>
  );
};
