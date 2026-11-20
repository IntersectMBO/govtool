import { useState } from "react";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { Box, ButtonBase, Skeleton } from "@mui/material";
import { formatDistanceToNowStrict } from "date-fns";
import {
  CircleCheck,
  CircleX,
  Clock3,
  FileCheck2,
  MessageCircle,
  MessagesSquare,
  Server,
  ShieldCheck,
  UserRound,
} from "lucide-react";

import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";
import { PDF_PATHS } from "@/consts/paths";
import { useFeatureFlag } from "@/context/featureFlag";
import { useModal } from "@/context/modal";
import { useCardano } from "@/context/wallet";
import type { GovernanceActionRecord, GovernanceActionVoteAggregate } from "@/models/governanceActions";
import { formatDisplayDate } from "@/utils/formatDate";
import {
  lifecycleEvents,
  outcomeKey,
  roleSummary,
  voterGroups,
  type VoterRole,
} from "@/utils/outcomeDetails";

import { Card, CardTitle, compactValue, Divider } from "./parts";
import { SupportingLinksCard, type Reference } from "./OverviewTab";

/** The forum proposal linked to an action, as the forum backend serves it. */
export type ForumProposal = {
  id: number | string;
  attributes?: {
    prop_comments_number?: number;
    content?: { attributes?: { prop_name?: string } };
  };
};

// ---- Discussions ---------------------------------------------------------

/** The action's forum discussion, or why there is none, and how to join in. */
export const DiscussionsTab = ({
  proposal,
  isLoading,
  references,
}: {
  proposal?: ForumProposal;
  isLoading: boolean;
  references: Reference[];
}) => {
  const { t } = useTranslation();
  const { isEnabled } = useCardano();
  const { openModal } = useModal();
  const { isProposalDiscussionForumEnabled } = useFeatureFlag();
  const comments = proposal?.attributes?.prop_comments_number ?? 0;
  const discussionPath = proposal ? `${PDF_PATHS.proposalDiscussion}/${proposal.id}` : undefined;

  let cta: { label: string; onClick?: () => void; to?: string } | undefined;
  // A linked proposal is public, so anyone may open it; joining needs a wallet.
  if (discussionPath) cta = { label: t("outcome.discussions.open"), to: discussionPath };
  else if (!isEnabled) cta = { label: t("outcome.discussions.ctaConnect"), onClick: () => openModal({ type: "chooseWallet" }) };
  else if (isProposalDiscussionForumEnabled) cta = { label: t("outcome.discussions.ctaForum"), to: PDF_PATHS.proposalDiscussion };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Card testId="outcome-discussion" sx={{ alignItems: "center", display: "flex", flexDirection: "column", justifyContent: "center", minHeight: { xxs: 200, md: 300 }, textAlign: "center" }}>
        {isLoading ? (
          <Skeleton height={80} variant="rounded" width="60%" />
        ) : (
          <>
            <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["800"], display: "flex", height: 36, justifyContent: "center", mb: 1.5, width: 36 }}>
              <MessagesSquare size={18} />
            </Box>
            <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0 }}>
              {proposal?.attributes?.content?.attributes?.prop_name || t("outcome.discussions.forumTitle")}
            </Box>
            <Box sx={{ color: ds.gray["600"], fontSize: 14, mt: 0.75 }}>
              {proposal ? t("outcome.discussions.linked") : t("outcome.discussions.empty")}
            </Box>
            {proposal && (
              <Box sx={{ alignItems: "center", color: ds.gray["700"], display: "flex", fontSize: 13, gap: 0.5, mt: 1 }}>
                <MessageCircle aria-hidden color={ds.orange["700"]} size={14} />
                {t("outcome.discussions.comments", { count: comments })}
              </Box>
            )}
          </>
        )}
      </Card>
      {cta && (
        <Box
          data-testid="outcome-discussion-cta"
          sx={{
            alignItems: { xxs: "flex-start", md: "center" },
            background: `linear-gradient(110deg, ${ds.primary["700"]} 0%, ${ds.primary["600"]} 75%, ${ds.primary["400"]} 100%)`,
            borderRadius: `${RADIUS.lg}px`,
            color: "#FFFFFF",
            display: "flex",
            flexDirection: { xxs: "column", md: "row" },
            gap: 2,
            justifyContent: "space-between",
            px: 3,
            py: 2.75,
          }}
        >
          <Box>
            <Box component="h2" sx={{ fontSize: 16, fontWeight: 600, m: 0 }}>{t("outcome.discussions.ctaTitle")}</Box>
            <Box sx={{ fontSize: 13, lineHeight: "20px", maxWidth: 340, mt: 1, opacity: 0.9 }}>{t("outcome.discussions.ctaBody")}</Box>
          </Box>
          <ButtonBase
            {...(cta.to ? { component: RouterLink, to: cta.to } : { onClick: cta.onClick })}
            data-testid={proposal && cta.to ? `proposal-${proposal.id}-view-details` : "outcome-discussion-cta-button"}
            sx={{ backgroundColor: "#FFFFFF", borderRadius: `${RADIUS.md}px`, color: ds.primary["700"], flexShrink: 0, fontFamily: "inherit", fontSize: 14, fontWeight: 600, px: 2, py: 1.25, textDecoration: "none", "&.Mui-focusVisible": { outline: "2px solid #FFFFFF", outlineOffset: 2 } }}
          >
            {cta.label}
          </ButtonBase>
        </Box>
      )}
      <SupportingLinksCard references={references} />
    </Box>
  );
};

// ---- Votes ---------------------------------------------------------------

const ROLE_ROWS: Record<VoterRole, { name: string; icon: typeof UserRound }> = {
  drep: { name: "outcome.breakdown.dreps", icon: UserRound },
  spo: { name: "outcome.breakdown.spos", icon: Server },
  cc: { name: "outcome.breakdown.cc", icon: ShieldCheck },
};

const BreakdownBar = ({ aggregate }: { aggregate: GovernanceActionVoteAggregate }) => {
  const summary = roleSummary(aggregate);
  if (!summary) return null;
  const noShare = summary.ratification > 0
    ? Math.round((Number(aggregate.no) / summary.ratification) * 10000) / 100
    : 0;
  return (
    <Box aria-hidden sx={{ backgroundColor: ds.gray["100"], backgroundImage: `repeating-linear-gradient(135deg, ${ds.gray["200"]} 0 2px, transparent 2px 5px)`, borderRadius: "3px", display: "flex", height: 6, overflow: "hidden" }}>
      <Box sx={{ backgroundColor: ds.success["600"], width: `${summary.yesPercent}%` }} />
      <Box sx={{ backgroundColor: ds.error["600"], width: `${noShare}%` }} />
    </Box>
  );
};

/** Each voting group's result, then the per-voter list. */
export const VotesTab = ({ action }: { action: GovernanceActionRecord }) => {
  const { t } = useTranslation();
  const [role, setRole] = useState<VoterRole>("drep");
  const outcome = outcomeKey(action);
  const groups = voterGroups(action);

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Card testId="outcome-breakdown" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <CardTitle>{t("outcome.breakdown.title")}</CardTitle>
        {groups.map((group, index) => {
          const { name, icon: Icon } = ROLE_ROWS[group.role];
          const summary = group.aggregate ? roleSummary(group.aggregate) : undefined;
          let chip: { label: string; tone: keyof typeof PILL_TONES } = { label: t("outcome.breakdown.notRequired"), tone: "neutral" };
          if (group.applicable) {
            chip = outcome === "live"
              ? { label: t("outcome.breakdown.active"), tone: "primary" }
              : { label: t("outcome.breakdown.complete"), tone: "success" };
          }
          // Members' votes, worded as the committee's; DRep and SPO counts are yes and no.
          const isCount = group.role === "cc" && group.aggregate?.representation === "count";
          return (
            <Box key={group.role} sx={{ display: "flex", flexDirection: "column", gap: 1.25 }}>
              {index > 0 && <Divider />}
              <Box sx={{ alignItems: "center", display: "flex", gap: 1, justifyContent: "space-between" }}>
                <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 14, fontWeight: 600, gap: 1 }}>
                  <Icon aria-hidden color={group.role === "spo" ? ds.orange["600"] : ds.primary["600"]} size={16} />
                  {t(name)}
                </Box>
                <Box component="span" sx={{ backgroundColor: PILL_TONES[chip.tone].bg, borderRadius: `${RADIUS.xs}px`, color: PILL_TONES[chip.tone].text, fontSize: 12, fontWeight: 500, px: 0.75, py: 0.25 }}>
                  {chip.label}
                </Box>
              </Box>
              {!group.applicable && (
                <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>
                  {t(group.role === "spo" ? "outcome.votes.spoNotVoting" : "outcome.votes.ccNotVoting")}
                </Box>
              )}
              {group.applicable && group.aggregate && summary && (
                <>
                  <Box sx={{ color: ds.gray["600"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 2 }}>
                    {isCount ? (
                      <>
                        <span>{`${t("outcome.votes.constitutional")}: `}<Box component="strong" sx={{ color: ds.gray["900"] }}>{`${group.aggregate.yes} (${summary.yesPercent.toFixed(2)}%)`}</Box></span>
                        <span>{`${t("outcome.votes.unconstitutional")}: `}<Box component="strong" sx={{ color: ds.gray["900"] }}>{group.aggregate.no}</Box></span>
                        <Box component="span" sx={{ color: ds.orange["700"] }}>{t("outcome.votes.membersThreshold", { percent: Math.round(summary.thresholdPercent) })}</Box>
                      </>
                    ) : (
                      <>
                        <span>{`${t("outcome.breakdown.yesPower")} `}<Box component="strong" sx={{ color: ds.success["700"] }}>{`${compactValue(summary.yes, group.aggregate.representation)} (${summary.yesPercent.toFixed(2)}%)`}</Box></span>
                        <span>{`${t("outcome.breakdown.noPower")} `}<Box component="strong" sx={{ color: ds.error["700"] }}>{`${compactValue(summary.rest, group.aggregate.representation)} (${summary.restPercent.toFixed(2)}%)`}</Box></span>
                      </>
                    )}
                  </Box>
                  <BreakdownBar aggregate={group.aggregate} />
                </>
              )}
              {group.applicable && !summary && (
                <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("outcome.votes.unavailable")}</Box>
              )}
            </Box>
          );
        })}
      </Card>
      <Card testId="outcome-voters" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <Box aria-label={t("outcome.voters.title")} role="tablist" sx={{ borderBottom: `1px solid ${ds.gray["200"]}`, display: "flex", gap: 1 }}>
          {(["drep", "spo", "cc"] as const).map((key) => (
            <ButtonBase
              key={key}
              aria-selected={role === key}
              onClick={() => setRole(key)}
              role="tab"
              sx={{ borderBottom: `2px solid ${role === key ? ds.primary["600"] : "transparent"}`, color: role === key ? ds.primary["700"] : ds.gray["600"], fontFamily: "inherit", fontSize: 14, fontWeight: role === key ? 600 : 500, mb: "-1px", px: 1.5, py: 1, "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}` } }}
            >
              {t(`outcome.voters.${key === "drep" ? "dreps" : key === "spo" ? "spos" : "cc"}`)}
            </ButtonBase>
          ))}
        </Box>
        <Box role="status" sx={{ color: ds.gray["600"], fontSize: 14, py: 3, textAlign: "center" }}>
          {t("outcome.voters.unavailable")}
        </Box>
      </Card>
    </Box>
  );
};

/** Forum comments and when the action last moved. */
export const AdditionalInfoCard = ({
  action,
  proposal,
}: {
  action: GovernanceActionRecord;
  proposal?: ForumProposal;
}) => {
  const { t } = useTranslation();
  const events = lifecycleEvents(action);
  const last = events[events.length - 1];
  const lastDate = last?.time ? new Date(last.time) : undefined;
  const comments = proposal?.attributes?.prop_comments_number;
  return (
    <Card testId="outcome-additional-information" sx={{ borderTop: `3px solid ${ds.primary["300"]}`, display: "flex", flexDirection: "column", gap: 2 }}>
      <CardTitle>{t("outcome.additional.title")}</CardTitle>
      <Box>
        <Box sx={{ color: ds.gray["600"], fontSize: 11, fontWeight: 600, letterSpacing: "0.3px", textTransform: "uppercase" }}>{t("outcome.additional.discussions")}</Box>
        <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 13, gap: 0.75, mt: 0.5 }}>
          <MessageCircle aria-hidden color={ds.orange["700"]} size={14} />
          {comments === undefined ? t("outcome.additional.none") : t("outcome.discussions.comments", { count: comments })}
        </Box>
      </Box>
      <Box>
        <Box sx={{ color: ds.gray["600"], fontSize: 11, fontWeight: 600, letterSpacing: "0.3px", textTransform: "uppercase" }}>{t("outcome.additional.lastActivity")}</Box>
        <Box sx={{ alignItems: "center", color: ds.gray["900"], display: "flex", fontSize: 13, gap: 0.75, mt: 0.5 }}>
          <Clock3 aria-hidden color={ds.gray["600"]} size={14} />
          {lastDate && !Number.isNaN(lastDate.getTime())
            ? formatDistanceToNowStrict(lastDate, { addSuffix: true })
            : t("outcome.additional.none")}
        </Box>
      </Box>
    </Card>
  );
};

// ---- Activity ------------------------------------------------------------

const EVENT_STYLE = {
  submitted: { icon: FileCheck2, color: ds.primary["600"] },
  ratified: { icon: CircleCheck, color: ds.success["700"] },
  enacted: { icon: CircleCheck, color: ds.success["700"] },
  expired: { icon: CircleX, color: ds.error["700"] },
  dropped: { icon: CircleX, color: ds.error["700"] },
};

/** The action's lifecycle as a timeline. */
export const ActivityTab = ({ action }: { action: GovernanceActionRecord }) => {
  const { t } = useTranslation();
  const events = lifecycleEvents(action);
  return (
    <Card testId="outcome-activity" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <CardTitle>{t("outcome.activity.title")}</CardTitle>
      <Box component="ol" sx={{ listStyle: "none", m: 0, p: 0 }}>
        {events.map((event, index) => {
          const { icon: Icon, color } = EVENT_STYLE[event.key];
          return (
            <Box key={event.key} component="li" sx={{ display: "flex", gap: 1.5, pb: index < events.length - 1 ? 2.5 : 0, position: "relative" }}>
              {index < events.length - 1 && (
                <Box aria-hidden sx={{ borderLeft: `2px solid ${ds.gray["200"]}`, bottom: 0, left: 15, position: "absolute", top: 32 }} />
              )}
              <Box aria-hidden sx={{ alignItems: "center", backgroundColor: ds.gray["50"], border: `1px solid ${ds.gray["200"]}`, borderRadius: "50%", color, display: "flex", flexShrink: 0, height: 32, justifyContent: "center", width: 32 }}>
                <Icon size={16} />
              </Box>
              <Box sx={{ pt: 0.5 }}>
                <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{t(`outcome.activity.${event.key}`)}</Box>
                <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>
                  {[formatDisplayDate(event.time, "MMM d, yyyy, hh:mm a"), t("outcome.activity.epoch", { epoch: event.epoch })].filter(Boolean).join(" · ")}
                </Box>
              </Box>
            </Box>
          );
        })}
      </Box>
    </Card>
  );
};
