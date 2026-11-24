import { Box, Skeleton } from "@mui/material";
import { Link } from "react-router";
import { AlarmClock, Check, CircleDot, CircleX } from "lucide-react";

import { GOV_ACTION_HISTORY_PATHS, GOV_ACTION_HISTORY_TYPE_FILTERS } from "@consts";
import { useGetGovernanceActionMetadata, useTranslation } from "@hooks";
import { GovernanceActionRecord } from "@models";
import {
  formatDisplayDate,
  getFullGovActionId,
  getGovernanceActionCIP129Id,
  getGovernanceActionProposalStatus,
  getGovernanceActionVoteEnd,
} from "@utils";
import { ds, FONT_FAMILY, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { voteBar } from "@/utils/governanceActionFormat";

type GovernanceActionHistoryCardProps = {
  action: GovernanceActionRecord;
};

const OUTCOME_STYLE = {
  Enacted: { tone: PILL_TONES.success, accent: ds.success["300"], Icon: Check },
  Ratified: { tone: PILL_TONES.success, accent: ds.success["300"], Icon: Check },
  Expired: { tone: PILL_TONES.error, accent: ds.error["300"], Icon: AlarmClock },
  "Not Ratified": { tone: PILL_TONES.error, accent: ds.error["300"], Icon: CircleX },
  Live: { tone: PILL_TONES.primary, accent: ds.primary["300"], Icon: CircleDot },
};

const mono = { fontFamily: FONT_FAMILY.mono, fontSize: 11, letterSpacing: "0.2px", lineHeight: "16px" };

type DateItemProps = { label: string; date: string | null; epoch: number | null };

const DateItem = ({ label, date, epoch }: DateItemProps) => {
  const { t } = useTranslation();
  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 0.25 }}>
      <Box component="span" sx={{ color: ds.gray["500"], fontSize: 11, fontWeight: 500, lineHeight: "16px", textTransform: "uppercase" }}>
        {label}
      </Box>
      <Box component="span" sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 500, lineHeight: "18px" }}>
        {[formatDisplayDate(date, "MMM d, yyyy"), epoch !== null ? t("govActions.card.epoch", { epoch }) : ""].filter(Boolean).join(" · ")}
      </Box>
    </Box>
  );
};

/** A concluded (or live) action in the Outcomes list. */
export const GovernanceActionHistoryCard = ({ action }: GovernanceActionHistoryCardProps) => {
  const { metadata, metadataValid, isMetadataLoading } =
    useGetGovernanceActionMetadata(action);
  const { t } = useTranslation();

  const idCIP129 = getGovernanceActionCIP129Id(action);
  const fullGovActionId = getFullGovActionId(action.tx_hash, action.index);
  const outcome = getGovernanceActionProposalStatus(action.status);
  const voteEnd = getGovernanceActionVoteEnd(action);
  const style = OUTCOME_STYLE[outcome];

  const title = action.title || metadata?.data?.title;
  const isDataMissing = metadata?.metadataStatus;
  const typeLabel =
    GOV_ACTION_HISTORY_TYPE_FILTERS.find((filter) => filter.value === action.type)
      ?.label || action.type;
  const isInfoAction = action.type === "InfoAction";
  // An enacted action's vote ended at ratification, an epoch earlier.
  const voteEndLabel =
    voteEnd?.outcome === "Enacted" && action.status.ratified_epoch !== null
      ? "Ratified"
      : voteEnd?.outcome.replace(" ", "");
  const tallies = (action.vote_aggregates ?? [])
    .map((aggregate) => ({ aggregate, bar: voteBar(aggregate) }))
    .filter(({ bar }) => bar !== undefined);

  return (
    <Box
      component="article"
      data-testid={`${idCIP129}-governance-action-card`}
      id={`${idCIP129}-governance-action-card`}
      sx={{
        backgroundColor: metadataValid ? "#FFFFFF" : "#FFFBFA",
        border: `1px solid ${ds.gray["200"]}`,
        borderLeft: `3px solid ${style.accent}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 1.25,
        minWidth: 0,
        p: { xxs: 2, md: 2.5 },
        position: "relative",
        "&:hover": { borderColor: ds.gray["300"], borderLeftColor: style.accent },
        "&:focus-within": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
      }}
    >
      <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5, minWidth: 0 }}>
          <Box component="span" data-testid={`${idCIP129}-type`} sx={{ ...mono, color: ds.gray["500"], textTransform: "uppercase" }}>
            {typeLabel}
          </Box>
          <Box
            component={Link}
            data-testid={`${fullGovActionId}-view-details`}
            to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${idCIP129}`}
            sx={{
              color: isDataMissing ? ds.error["600"] : ds.gray["900"],
              fontSize: 16,
              fontWeight: 600,
              letterSpacing: "-0.18px",
              lineHeight: "24px",
              outline: "none",
              textDecoration: "none",
              wordBreak: "break-word",
              "&::after": { content: '""', inset: 0, position: "absolute" },
              "&:hover": { color: ds.primary["600"] },
            }}
          >
            <span data-testid={`${idCIP129}-card-title`}>
              {isMetadataLoading && !title ? (
                <Skeleton component="span" sx={{ display: "inline-block" }} variant="text" width={240} />
              ) : (
                title || t("govActions.card.untitled")
              )}
            </span>
          </Box>
        </Box>
        <Box
          component="span"
          data-testid={`${idCIP129}-status`}
          sx={{
            alignItems: "center",
            backgroundColor: style.tone.bg,
            border: `1px solid ${style.tone.border}`,
            borderRadius: `${RADIUS.pill}px`,
            color: style.tone.text,
            display: "inline-flex",
            flexShrink: 0,
            fontSize: 12,
            fontWeight: 500,
            gap: 0.5,
            lineHeight: "18px",
            px: 1,
          }}
        >
          <style.Icon aria-hidden size={12} strokeWidth={2.25} />
          {t(`governanceActionHistoryList.outcome.${outcome.replace(" ", "")}`)}
        </Box>
      </Box>

      {tallies.length > 0 && (
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
          {tallies.map(({ aggregate, bar }, position) => (
            <Box key={aggregate.role} component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 1 }}>
              {position > 0 && <Box aria-hidden component="span" sx={{ borderLeft: `1px solid ${ds.gray["300"]}`, height: 10 }} />}
              <Box component="span" data-testid={`${idCIP129}-${aggregate.role}-tally`} sx={{ ...mono, alignItems: "center", color: ds.gray["700"], display: "inline-flex", gap: 0.75 }}>
                <Box aria-hidden component="span" sx={{ backgroundColor: isInfoAction ? ds.gray["400"] : bar?.passing ? ds.success["600"] : ds.error["600"], borderRadius: "2px", height: 8, width: 8 }} />
                {`${t(`governanceActionHistoryList.roles.${aggregate.role}`).toUpperCase()} ${bar?.yesPercent.toFixed(1)}%`}
              </Box>
            </Box>
          ))}
        </Box>
      )}

      <Box sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />

      <Box sx={{ display: "flex", flexWrap: "wrap", gap: { xxs: 2, sm: 3 } }}>
        <DateItem date={action.time} epoch={action.epoch_no} label={t("govActions.card.submitted")} />
        <Box aria-hidden sx={{ alignSelf: "center", borderLeft: `1px solid ${ds.gray["200"]}`, height: 24 }} />
        {voteEnd ? (
          <DateItem
            date={voteEnd.time}
            epoch={voteEnd.epoch}
            label={t(`governanceActionHistoryList.outcome.${voteEndLabel}`)}
          />
        ) : (
          <DateItem date={action.expiry_date} epoch={action.expiration} label={t("govActions.card.expires")} />
        )}
      </Box>
    </Box>
  );
};
