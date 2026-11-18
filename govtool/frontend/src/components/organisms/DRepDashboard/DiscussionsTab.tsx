import { ReactNode, useState } from "react";
import { Box, Skeleton } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { ArrowRight, MessageSquare, MessagesSquare, ThumbsUp } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { SortSelect } from "@/components/molecules/SortSelect";
import { PDF_PATHS } from "@/consts/paths";
import { ds, PILL_TONES, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useDRepDiscussions } from "@/hooks/queries/useDRepDashboard";
import type { DRepDiscussion } from "@/models";

import { ErrorState } from "./GovernanceTab";
import { SideColumn, twoColumns } from "./OverviewTab";
import { EmptyState, timeAgo } from "./parts";
import type { DashboardData } from "./types";

const PAGE_SIZE = 5;
const TOPIC_TONES = [
  PILL_TONES.primary,
  PILL_TONES.warning,
  PILL_TONES.success,
  PILL_TONES.error,
  PILL_TONES.neutral,
];

const topicTone = (topic: string) =>
  TOPIC_TONES[[...topic].reduce((sum, ch) => sum + ch.charCodeAt(0), 0) % TOPIC_TONES.length];

const ThreadCard = ({ thread }: { thread: DRepDiscussion }) => {
  const { t } = useTranslation();
  const tone = thread.topic ? topicTone(thread.topic) : undefined;
  return (
    <Box component="li" data-testid="drep-discussion" sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, boxShadow: SHADOWS.card, display: "flex", flexDirection: "column", gap: 1.25, p: { xxs: 2, md: 2.5 } }}>
      <Box sx={{ alignItems: "flex-start", display: "flex", gap: 1.5, justifyContent: "space-between" }}>
        <Box
          component={RouterLink}
          sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, lineHeight: "24px", overflowWrap: "anywhere", textDecoration: "none", "&:hover": { color: ds.primary["600"], textDecoration: "underline" } }}
          to={PDF_PATHS.proposalDiscussionProposal.replace(":id", String(thread.id))}
        >
          {thread.title || t("drepDashboard.discussions.untitled")}
        </Box>
        {thread.topic && tone && (
          <Box component="span" sx={{ backgroundColor: tone.bg, border: `1px solid ${tone.border}`, borderRadius: `${RADIUS.xs}px`, color: tone.text, flexShrink: 0, fontSize: 12, fontWeight: 600, letterSpacing: "0.3px", px: 0.75, textTransform: "uppercase" }}>
            {thread.topic}
          </Box>
        )}
      </Box>
      <Box sx={{ alignItems: "center", color: ds.gray["600"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 0.75 }}>
        <span>
          {t("drepDashboard.discussions.postedBy", { name: thread.author || t("drepDashboard.discussions.anonymous") })}
          {thread.isOwn && ` ${t("drepDashboard.discussions.you")}`}
          {thread.createdAt && ` · ${timeAgo(thread.createdAt)}`}
        </span>
        {thread.repliedByYou && !thread.isOwn && (
          <Box component="span" sx={{ backgroundColor: PILL_TONES.success.bg, borderRadius: `${RADIUS.xs}px`, color: PILL_TONES.success.text, fontSize: 12, fontWeight: 500, px: 0.75 }}>
            {t("drepDashboard.discussions.youReplied")}
          </Box>
        )}
      </Box>
      {thread.abstract && (
        <Box component="p" sx={{ color: ds.gray["600"], display: "-webkit-box", fontSize: 14, lineHeight: "20px", m: 0, overflow: "hidden", WebkitBoxOrient: "vertical", WebkitLineClamp: 2 }}>
          {thread.abstract}
        </Box>
      )}
      <Box sx={{ alignItems: "center", color: ds.gray["600"], display: "flex", flexWrap: "wrap", fontSize: 13, gap: 2, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", gap: 2 }}>
          <Box component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 0.5 }}>
            <MessageSquare aria-hidden size={14} />
            {t("drepDashboard.discussions.replies", { count: thread.replies })}
          </Box>
          <Box component="span" sx={{ alignItems: "center", display: "inline-flex", gap: 0.5 }}>
            <ThumbsUp aria-hidden size={14} />
            {t("drepDashboard.discussions.likes", { count: thread.likes })}
          </Box>
        </Box>
        {thread.lastReply && <span>{t("drepDashboard.discussions.lastReply", { ago: timeAgo(thread.lastReply.at) })}</span>}
      </Box>
    </Box>
  );
};

const Threads = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const { data: threads, isLoading, isError, refetch } = useDRepDiscussions(data.dRepID);
  const [topic, setTopic] = useState("all");
  const [sort, setSort] = useState("recent");
  const [page, setPage] = useState(1);
  const all = threads ?? [];
  const topics = [
    ...new Set(
      all.map((thread) => thread.topic).filter((value): value is string => !!value),
    ),
  ];
  const rows = all
    .filter((thread) => topic === "all" || thread.topic === topic)
    .sort((a, b) => (sort === "replies" ? b.replies - a.replies : 0));
  const visible = rows.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);

  let body: ReactNode;
  if (isLoading) body = <Skeleton height={220} variant="rounded" />;
  else if (isError) body = <ErrorState message={t("drepDashboard.discussions.error")} onRetry={() => { refetch(); }} />;
  else if (!all.length) {
    body = (
      <Box sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px` }}>
        <EmptyState
          actions={<Button component={RouterLink} endIcon={<ArrowRight aria-hidden size={16} />} size="medium" to={PDF_PATHS.proposalDiscussion}>{t("drepDashboard.discussions.start")}</Button>}
          body={t("drepDashboard.discussions.emptyBody")}
          framed={false}
          icon={<MessagesSquare size={20} />}
          testId="drep-discussions-empty"
          title={t("drepDashboard.discussions.emptyTitle")}
        />
      </Box>
    );
  } else {
    body = (
      <>
        <Box component="ul" sx={{ display: "flex", flexDirection: "column", gap: 2, listStyle: "none", m: 0, p: 0 }}>
          {visible.map((thread) => <ThreadCard key={thread.id} thread={thread} />)}
        </Box>
        {rows.length > PAGE_SIZE && (
          <PaginationFooter
            onPageChange={setPage}
            onPageSizeChange={() => {}}
            page={page}
            pageSize={PAGE_SIZE}
            total={rows.length}
          />
        )}
      </>
    );
  }

  return (
    <Box component="section" data-testid="drep-discussions" sx={{ display: "flex", flexDirection: "column", gap: 2, minWidth: 0 }}>
      <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
        <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
          <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("drepDashboard.discussions.title")}</Box>
          {!isLoading && !isError && (
            <Box component="span" sx={{ backgroundColor: PILL_TONES.primary.bg, borderRadius: `${RADIUS.pill}px`, color: PILL_TONES.primary.text, fontSize: 12, fontWeight: 500, px: 1 }}>
              {t("drepDashboard.discussions.count", { count: all.length })}
            </Box>
          )}
        </Box>
        {all.length > 0 && (
          <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5 }}>
            <SortSelect
              dataTestId="drep-discussions-topic"
              label={t("drepDashboard.discussions.topic")}
              onChange={(next) => { setTopic(next); setPage(1); }}
              options={[{ key: "all", label: t("drepDashboard.discussions.allTopics") }, ...topics.map((value) => ({ key: value, label: value }))]}
              value={topic}
            />
            <SortSelect
              dataTestId="drep-discussions-sort"
              label={t("drepDashboard.sortBy")}
              onChange={(next) => { setSort(next); setPage(1); }}
              options={[{ key: "recent", label: t("drepDashboard.discussions.mostRecent") }, { key: "replies", label: t("drepDashboard.discussions.mostReplies") }]}
              value={sort}
            />
          </Box>
        )}
      </Box>
      {body}
      <Box sx={{ color: ds.gray["500"], fontSize: 13 }}>{t("drepDashboard.discussions.note")}</Box>
    </Box>
  );
};

export const DiscussionsTab = ({
  data,
  walletCard,
}: {
  data: DashboardData;
  walletCard: ReactNode;
}) => (
  <Box sx={twoColumns}>
    <SideColumn data={data} full={false} walletCard={walletCard} />
    <Threads data={data} />
  </Box>
);
