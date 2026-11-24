import { ReactNode, useMemo, useState } from "react";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase } from "@mui/material";
import { ExternalLink } from "lucide-react";
import Markdown from "react-markdown";
import rehypeKatex from "rehype-katex";
import remarkGfm from "remark-gfm";
import remarkMath from "remark-math";
import "katex/dist/katex.min.css";

import { GovernanceActionDetailsDiffView } from "@/components/molecules/GovernanceActionDetailsDiffView";
import { GovernanceActionHardForkDetails } from "@/components/molecules/GovernanceActionHardForkDetails";
import { GovernanceActionNewCommitteeDetails } from "@/components/molecules/GovernanceActionNewCommitteeDetails";
import { GovernanceActionNewConstitutionDetailsTabContent } from "@/components/molecules/GovernanceActionNewConstitutionDetailsTabContent";
import { ds, RADIUS } from "@/consts/designTokens";
import type { EpochParams } from "@/models/api";
import type { GovernanceActionRecord } from "@/models/governanceActions";
import { GovernanceActionType } from "@/types/governanceAction";
import { filterOutNullParams } from "@/utils/filterOutNullParams";
import { filterUpdatableProtocolParams } from "@/utils/filterUpdatableProtocolParams";
import { getFullGovActionId } from "@/utils/getGovActionId";
import { mapArrayToObjectByKeys } from "@/utils/mapArrayToObjectByKeys";
import { markdownLineBreaks } from "@/utils/markdownLineBreaks";
import { openInNewTab } from "@/utils/openInNewTab";

import { Card, CardTitle } from "./parts";

const IGNORED_KEYS = ["id", "registered_tx_id", "key"];

export type Reference = { "@type"?: string; label?: string; uri: string };

const Section = ({ title, text, testId }: { title: string; text?: string; testId: string }) =>
  (text ? (
    <Box data-testid={testId}>
      <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, m: 0 }}>{title}</Box>
      <Box
        sx={{
          color: ds.gray["600"],
          fontSize: 14,
          lineHeight: "22px",
          mt: 0.75,
          overflowWrap: "anywhere",
          "& p": { m: 0, mb: 1 },
          "& a": { color: ds.primary["600"] },
          "& img": { maxWidth: "100%" },
        }}
      >
        {/* Markdown without raw HTML: react-markdown escapes it. Line breaks and
            maths as the rest of GovTool renders them. */}
        <Markdown
          rehypePlugins={[rehypeKatex]}
          // A single $ is an amount, not maths; maths sits between $$.
          remarkPlugins={[[remarkMath, { singleDollarTextMath: false }], remarkGfm]}
        >
          {markdownLineBreaks(text)}
        </Markdown>
      </Box>
    </Box>
  ) : null);

/** Abstract, motivation and rationale; long text folds behind Read more. */
type ReasoningProps = { abstract?: string; motivation?: string; rationale?: string };

const ReasoningCard = ({ abstract, motivation, rationale }: ReasoningProps) => {
  const { t } = useTranslation();
  const [isOpen, setIsOpen] = useState(false);
  const length = (abstract?.length ?? 0) + (motivation?.length ?? 0) + (rationale?.length ?? 0);
  const isLong = length > 1500;
  if (!abstract && !motivation && !rationale) {
    return (
      <Card testId="outcome-reasoning">
        <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{t("outcome.noReasoning")}</Box>
      </Card>
    );
  }
  return (
    <Card testId="outcome-reasoning">
      <Box
        sx={{
          display: "flex",
          flexDirection: "column",
          gap: 2,
          maxHeight: isLong && !isOpen ? 520 : "none",
          overflow: "hidden",
          position: "relative",
          ...(isLong && !isOpen && {
            "&::after": {
              background: "linear-gradient(180deg, rgba(255,255,255,0) 0%, #FFFFFF 100%)",
              bottom: 0,
              content: '""',
              height: 64,
              left: 0,
              position: "absolute",
              right: 0,
            },
          }),
        }}
      >
        <Section testId="governance-action-abstract" text={abstract} title={t("outcome.abstract")} />
        <Section testId="governance-action-motivation" text={motivation} title={t("outcome.motivation")} />
        <Section testId="governance-action-rationale" text={rationale} title={t("outcome.rationale")} />
      </Box>
      {isLong && (
        <ButtonBase
          aria-expanded={isOpen}
          onClick={() => setIsOpen((open) => !open)}
          sx={{ color: ds.primary["600"], fontFamily: "inherit", fontSize: 14, fontWeight: 600, mt: 1.5, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
        >
          {isOpen ? t("outcome.readLess") : t("outcome.readMore")}
        </ButtonBase>
      )}
    </Card>
  );
};

/** The action's supporting links, each opening in a new tab. */
export const SupportingLinksCard = ({ references }: { references: Reference[] }) => {
  const { t } = useTranslation();
  return (
    <Card testId="outcome-supporting-links" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <CardTitle>{t("outcome.supportingLinks")}</CardTitle>
      {references.length ? references.map((reference, index) => (
        <Box key={`${reference.uri}-${reference.label ?? ""}`} sx={{ minWidth: 0 }}>
          {reference.label && <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{reference.label}</Box>}
          <ButtonBase
            data-testid={`supporting-link-${index + 1}`}
            onClick={() => openInNewTab(reference.uri)}
            sx={{ alignItems: "center", color: ds.primary["600"], fontFamily: "inherit", fontSize: 13, gap: 0.5, justifyContent: "flex-start", maxWidth: "100%", mt: 0.25, textAlign: "left", wordBreak: "break-all", "&:hover": { textDecoration: "underline" }, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            {reference.uri}
            <ExternalLink aria-hidden size={13} style={{ flexShrink: 0 }} />
          </ButtonBase>
        </Box>
      )) : (
        <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{t("outcome.noLinks")}</Box>
      )}
    </Card>
  );
};

type SectionKey = "reasoning" | "parameters" | "links";

/** Reasoning, the type's own details, and the supporting links. */
export const OverviewTab = ({
  action,
  epochParams,
  abstract,
  motivation,
  rationale,
  references,
}: {
  action: GovernanceActionRecord;
  epochParams?: EpochParams;
  abstract?: string;
  motivation?: string;
  rationale?: string;
  references: Reference[];
}) => {
  const { t } = useTranslation();
  const [section, setSection] = useState<SectionKey>("reasoning");
  const { type, description } = action;

  const proposed = useMemo(
    () => mapArrayToObjectByKeys(action.proposal_params, ["PlutusV1", "PlutusV2", "PlutusV3"]),
    [action.proposal_params],
  );
  const updatable = useMemo(
    () => filterUpdatableProtocolParams(epochParams, proposed, IGNORED_KEYS),
    [epochParams, proposed],
  );
  const nonNull = useMemo(() => filterOutNullParams(proposed, IGNORED_KEYS), [proposed]);
  const prevIndex = action.prev_gov_action_index;
  const prevId = prevIndex !== null && prevIndex !== undefined && action.prev_gov_action_tx_hash
    ? getFullGovActionId(action.prev_gov_action_tx_hash, prevIndex)
    : null;

  let parameters: ReactNode = null;
  const hasParams =
    type === GovernanceActionType.ParameterChange || type === GovernanceActionType.NewConstitution;
  if (hasParams &&
    action.proposal_params && epochParams) {
    parameters = <GovernanceActionDetailsDiffView newJson={nonNull} oldJson={updatable} />;
  } else if (type === GovernanceActionType.HardForkInitiation && description) {
    parameters = <GovernanceActionHardForkDetails action={action} prevGovActionId={prevId} />;
  } else if (type === GovernanceActionType.NewCommittee && description) {
    parameters = <GovernanceActionNewCommitteeDetails description={description} />;
  } else if (type === GovernanceActionType.NewConstitution && description?.anchor) {
    parameters = (
      <GovernanceActionNewConstitutionDetailsTabContent details={description ?? undefined} />
    );
  }

  const sections: SectionKey[] = parameters ? ["reasoning", "parameters", "links"] : ["reasoning", "links"];
  const current = sections.includes(section) ? section : "reasoning";

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Box
        aria-label={t("outcome.sections.label")}
        role="tablist"
        sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "grid", gap: 0.5, gridTemplateColumns: `repeat(${sections.length}, minmax(0, 1fr))`, p: 0.5 }}
      >
        {sections.map((key) => (
          <ButtonBase
            key={key}
            aria-selected={current === key}
            data-testid={`${key === "parameters" ? "parameters" : key}-tab`}
            onClick={() => setSection(key)}
            role="tab"
            sx={{ alignItems: "center", backgroundColor: current === key ? ds.gray["100"] : "transparent", borderRadius: `${RADIUS.sm}px`, color: ds.gray["900"], fontFamily: "inherit", fontSize: 14, fontWeight: current === key ? 600 : 500, gap: 1, justifyContent: "flex-start", px: 1.5, py: 1, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
          >
            <Box aria-hidden sx={{ backgroundColor: ds.gray["700"], borderRadius: "2px", height: 6, width: 6 }} />
            {t(`outcome.sections.${key}`)}
          </ButtonBase>
        ))}
      </Box>
      {current === "reasoning" && (
        <>
          <ReasoningCard abstract={abstract} motivation={motivation} rationale={rationale} />
          <SupportingLinksCard references={references} />
        </>
      )}
      {current === "parameters" && (
        <Card testId="outcome-parameters" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
          {parameters}
        </Card>
      )}
      {current === "links" && <SupportingLinksCard references={references} />}
    </Box>
  );
};
