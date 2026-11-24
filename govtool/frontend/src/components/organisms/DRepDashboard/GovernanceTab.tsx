import { ReactNode, useMemo, useState } from "react";
import { Box, ButtonBase, Dialog, Skeleton } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import {
  ArrowRight,
  ChevronDown,
  CircleCheck,
  CircleMinus,
  CircleX,
  FileText,
  Info,
  Landmark,
  Search,
  Send,
  Users,
  X,
} from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { SortSelect } from "@/components/molecules/SortSelect";
import { PATHS } from "@/consts/paths";
import { LINKS } from "@/consts/links";
import { ds, PILL_TONES, RADIUS } from "@/consts/designTokens";
import { useSnackbar } from "@/context/snackbar";
import { useGetAdaHolderCurrentDelegationQuery } from "@/hooks/queries/useGetAdaHolderCurrentDelegationQuery";
import { useGetAdaHolderVotingPowerQuery } from "@/hooks/queries/useGetAdaHolderVotingPowerQuery";
import { useGetDRepDetailsQuery } from "@/hooks/queries/useGetDrepDetailsQuery";
import { useGetVoteContextTextFromFile } from "@/hooks/queries/useGetVoteContextTextFromFile";
import {
  useDelegationHistory,
  useDRepDelegators,
} from "@/hooks/queries/useDRepDashboard";
import type { GovernanceActionRecord, VotedProposal } from "@/models";
import { AutomatedVotingOptionCurrentDelegation } from "@/types/automatedVotingOptions";
import { getFullGovActionId, getGovActionHistoryPath } from "@/utils/getGovActionId";
import { getProposalTypeLabel } from "@/utils/getProposalTypeLabel";
import { openInNewTab } from "@/utils/openInNewTab";
import {
  actionKey,
  actionOutcome,
  stakeHexToBech32,
  toCsv,
  voteSummary,
  type ActionOutcome,
} from "@/utils/drepDashboard";

import {
  downloadCsv,
  EmptyState,
  ExportButton,
  formatAda,
  formatShortDate,
  Initials,
  OUTCOME_TONES,
  Panel,
  Pill,
  shortId,
  TableFrame,
  td,
  th,
  timeAgo,
  UnderlineTabs,
  VOTE_COLORS,
  VOTE_TEXT,
  CopyButton,
} from "./parts";
import { sortedVotes } from "./OverviewTab";
import type { DashboardData } from "./types";

const PAGE_SIZE = 12;
const SUB_TABS = ["votes", "proposals", "delegators", "delegations"] as const;
type SubTab = (typeof SUB_TABS)[number];

const today = () => new Date().toISOString().slice(0, 10);

/** Totals and the yes / no / abstain bar. */
const VotingHistoryCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const summary = voteSummary(data.votes ?? []);
  const segments = [
    { key: "yes", count: summary.yes, color: VOTE_COLORS.yes },
    { key: "no", count: summary.no, color: VOTE_COLORS.no },
    { key: "abstain", count: summary.abstain, color: VOTE_COLORS.abstain },
  ] as const;
  return (
    <Panel testId="drep-voting-history">
      <Box sx={{ alignItems: "baseline", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
        <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>{t("drepDashboard.governance.history.title")}</Box>
        {data.votesLoading ? (
          <Skeleton width={260} />
        ) : (
          <Box data-testid="drep-voting-history-totals" sx={{ color: ds.gray["600"], fontSize: 14 }}>
            {t("drepDashboard.governance.history.totals", { total: summary.total })}
            {segments.map(({ key, count }) => (
              <Box key={key} component="span" sx={{ color: VOTE_TEXT[key], fontWeight: 600 }}>
                {" · "}
                {t(`drepDashboard.governance.history.${key}`, { count, pct: summary[`${key}Pct`] })}
              </Box>
            ))}
          </Box>
        )}
      </Box>
      <Box aria-hidden sx={{ backgroundColor: ds.gray["100"], borderRadius: `${RADIUS.pill}px`, display: "flex", height: 8, overflow: "hidden" }}>
        {summary.total > 0 && segments.map(({ key, count, color }) => (
          <Box key={key} sx={{ backgroundColor: color, flex: count, minWidth: count ? 2 : 0 }} />
        ))}
      </Box>
    </Panel>
  );
};

const SEGMENT_COLORS = [ds.orange["400"], ds.primary["400"], ds.orange["200"], ds.success["200"], ds.error["100"]];

/** The largest delegators' share of the DRep's voting power. */
const DistributionCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const { isServed, data: page } = useDRepDelegators(data.dRepID, 1, 10);
  const total = Number(data.voter.votingPower ?? 0);
  const rows = page?.elements ?? [];
  if (!isServed || !rows.length || !total) return null;
  const share = (lovelace: string) => (Number(lovelace) / total) * 100;
  const top = rows.slice(0, 5).map((row, index) => ({
    label: shortId(row.stakeAddress, 8, 0),
    pct: share(row.votingPower),
    color: SEGMENT_COLORS[index],
  }));
  const others = Math.max(0, 100 - top.reduce((sum, row) => sum + row.pct, 0));
  const topTen = rows.reduce((sum, row) => sum + share(row.votingPower), 0);
  const segments = [...top, { label: t("drepDashboard.governance.distribution.others"), pct: others, color: ds.gray["200"] }];
  return (
    <Panel
      subtitle={t("drepDashboard.governance.distribution.subtitle")}
      testId="drep-delegation-distribution"
      title={t("drepDashboard.governance.distribution.title")}
      titleId="drep-distribution-title"
    >
      <Box aria-hidden sx={{ borderRadius: `${RADIUS.xs}px`, display: "flex", gap: "2px", height: 24, overflow: "hidden" }}>
        {segments.map((segment) => (
          <Box
            key={segment.label}
            sx={{
              backgroundColor: segment.color,
              flex: segment.pct,
              minWidth: segment.pct ? 2 : 0,
            }}
          />
        ))}
      </Box>
      <Box component="ul" sx={{ display: "flex", flexWrap: "wrap", gap: 2, justifyContent: "space-between", listStyle: "none", m: 0, p: 0 }}>
        {segments.map((segment) => (
          <Box key={segment.label} component="li" sx={{ alignItems: "center", color: ds.gray["700"], display: "flex", fontSize: 13, gap: 0.75 }}>
            <Box aria-hidden component="span" sx={{ backgroundColor: segment.color, borderRadius: "50%", height: 8, width: 8 }} />
            {t("drepDashboard.governance.distribution.share", { name: segment.label, pct: segment.pct.toFixed(2) })}
          </Box>
        ))}
      </Box>
      <Box sx={{ alignItems: "center", borderTop: `1px solid ${ds.gray["200"]}`, color: ds.gray["800"], display: "flex", fontSize: 14, fontWeight: 500, gap: 1, pt: 2 }}>
        <Info aria-hidden color={ds.primary["600"]} size={16} />
        {t("drepDashboard.governance.distribution.topShare", { count: rows.length, pct: topTen.toFixed(1) })}
      </Box>
    </Panel>
  );
};

const VOTE_ICONS = { yes: CircleCheck, no: CircleX, abstain: CircleMinus };
const VOTE_TONES = { yes: PILL_TONES.primary, no: PILL_TONES.error, abstain: PILL_TONES.neutral };

const VoteChip = ({ vote }: { vote: "yes" | "no" | "abstain" }) => {
  const { t } = useTranslation();
  const Icon = VOTE_ICONS[vote];
  return (
    <Box component="span" sx={{ alignItems: "center", backgroundColor: VOTE_TONES[vote].bg, borderRadius: `${RADIUS.pill}px`, color: VOTE_TONES[vote].text, display: "inline-flex", fontSize: 13, fontWeight: 500, gap: 0.5, px: 1, py: 0.25 }}>
      <Icon aria-hidden size={14} />
      {t(`drepDashboard.vote.${vote}`)}
    </Box>
  );
};

const TypeChip = ({ type }: { type: string }) => (
  <Box component="span" sx={{ border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.pill}px`, color: ds.gray["700"], display: "inline-block", fontSize: 12, px: 1, py: 0.25, whiteSpace: "nowrap" }}>
    {getProposalTypeLabel(type)}
  </Box>
);

const OutcomePill = ({ outcome }: { outcome: ActionOutcome }) => {
  const { t } = useTranslation();
  if (outcome === "unknown") return null;
  return <Pill dot={false} label={t(`drepDashboard.outcome.${outcome}`)} tone={OUTCOME_TONES[outcome]} />;
};

/** The rationale a DRep attached to a vote, read from its anchor. */
const RationaleDialog = ({ row, onClose }: { row: VotedProposal; onClose: () => void }) => {
  const { t } = useTranslation();
  const [expanded, setExpanded] = useState(false);
  const context = useGetVoteContextTextFromFile(row.vote?.url, row.vote?.metadataHash);
  const { voteContextText, isLoading, valid } = context ?? {
    voteContextText: undefined,
    isLoading: true,
    valid: true,
  };
  const text = voteContextText ?? "";
  const isLong = text.length > 600;
  let body: ReactNode;
  if (isLoading) body = <Skeleton height={120} variant="rounded" />;
  else if (!valid) body = <Box role="alert" sx={{ color: ds.error["700"], fontSize: 14 }}>{t("drepDashboard.governance.rationale.invalid")}</Box>;
  else if (!text) body = <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{t("drepDashboard.governance.rationale.empty")}</Box>;
  else {
    body = (
      <>
        <Box data-testid="drep-rationale-text" sx={{ color: ds.gray["700"], fontSize: 14, lineHeight: "20px", overflowWrap: "anywhere", whiteSpace: "pre-line" }}>
          {isLong && !expanded ? `${text.slice(0, 600)}…` : text}
        </Box>
        {isLong && (
          <ButtonBase
            aria-expanded={expanded}
            onClick={() => setExpanded((open) => !open)}
            sx={{ alignSelf: "flex-start", border: `1px solid ${ds.primary["200"]}`, borderRadius: `${RADIUS.md}px`, color: ds.primary["600"], fontSize: 14, fontWeight: 600, gap: 0.5, px: 1.5, py: 0.75 }}
          >
            {expanded ? t("drepDashboard.governance.rationale.showLess") : t("drepDashboard.governance.rationale.showAll")}
            <ChevronDown aria-hidden size={16} style={{ transform: expanded ? "rotate(180deg)" : undefined }} />
          </ButtonBase>
        )}
      </>
    );
  }
  const vote = row.vote?.vote;
  return (
    <Dialog
      aria-labelledby="drep-rationale-title"
      onClose={onClose}
      open
      PaperProps={{ sx: { borderRadius: `${RADIUS.lg}px`, borderTop: `3px solid ${ds.primary["400"]}`, maxWidth: 800, p: { xxs: 2.5, md: 4 }, width: "100%" } }}
    >
      <Box data-testid="drep-rationale-dialog" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
        <Box sx={{ alignItems: "flex-start", display: "flex", gap: 2, justifyContent: "space-between" }}>
          <Box>
            <Box component="h2" id="drep-rationale-title" sx={{ color: ds.gray["900"], fontSize: 18, fontWeight: 600, m: 0 }}>
              {row.proposal.title || t("govActions.card.untitled")}
            </Box>
            <Box sx={{ color: ds.gray["600"], fontSize: 13, mt: 0.5 }}>
              {timeAgo(row.vote?.date)}
              {vote && (
                <>
                  {" • "}
                  {t("drepDashboard.governance.rationale.voted")}{" "}
                  <Box component="span" sx={{ color: VOTE_TEXT[vote], fontWeight: 600 }}>{t(`drepDashboard.vote.${vote}`)}</Box>
                </>
              )}
            </Box>
          </Box>
          <ButtonBase aria-label={t("drepDashboard.governance.rationale.close")} onClick={onClose} sx={{ borderRadius: "50%", color: ds.gray["700"], p: 0.5, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}>
            <X aria-hidden size={20} />
          </ButtonBase>
        </Box>
        <Box aria-hidden sx={{ borderTop: `1px dashed ${ds.gray["200"]}` }} />
        {body}
      </Box>
    </Dialog>
  );
};

const VOTE_FILTERS = ["all", "active", "ratified", "enacted", "expired", "dropped"] as const;

const MyVotes = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const [status, setStatus] = useState<string>("all");
  const [sort, setSort] = useState("newest");
  const [page, setPage] = useState(1);
  const [open, setOpen] = useState<VotedProposal | null>(null);

  const rows = useMemo(() => {
    const newest = sortedVotes(data).filter((row) => row.vote);
    const ordered = sort === "oldest" ? newest.reverse() : newest;
    return ordered
      .map((row) => ({
        row,
        outcome: actionOutcome(
          data.actionsByKey.get(actionKey(row.proposal.txHash, row.proposal.index)),
        ),
      }))
      .filter(({ outcome }) => status === "all" || outcome === status);
  }, [data, sort, status]);

  if (data.votesLoading) return <Skeleton height={320} variant="rounded" />;
  if (data.votesError) return <ErrorState message={t("drepDashboard.governance.votes.error")} onRetry={data.retryVotes} />;
  const hasVotes = (data.votes ?? []).some((row) => row.vote);
  const visible = rows.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);

  return (
    <SubPanel
      count={t("drepDashboard.governance.votes.showing", { count: rows.length })}
      filters={(
        <SortSelect
          dataTestId="drep-votes-status"
          label={t("drepDashboard.status")}
          onChange={(next) => { setStatus(next); setPage(1); }}
          options={VOTE_FILTERS.map((key) => ({ key, label: key === "all" ? t("drepDashboard.all") : t(`drepDashboard.outcome.${key}`) }))}
          value={status}
        />
      )}
      sort={(
        <SortSelect
          dataTestId="drep-votes-sort"
          label={t("drepDashboard.sortBy")}
          onChange={(next) => { setSort(next); setPage(1); }}
          options={[{ key: "newest", label: t("drepDashboard.governance.votes.newest") }, { key: "oldest", label: t("drepDashboard.governance.votes.oldest") }]}
          value={sort}
        />
      )}
    >
      {!hasVotes && (
        <EmptyState body={t("drepDashboard.governance.votes.emptyBody")} icon={<FileText size={20} />} testId="drep-votes-empty" title={t("drepDashboard.governance.votes.emptyTitle")} />
      )}
      {hasVotes && !rows.length && <Box sx={{ color: ds.gray["600"], fontSize: 14, py: 4, textAlign: "center" }}>{t("drepDashboard.governance.votes.noMatch")}</Box>}
      {rows.length > 0 && (
        <>
          <TableFrame label={t("drepDashboard.governance.activity.tabs.votes")} minWidth={860}>
            <thead>
              <tr>
                {[t("drepDashboard.governance.votes.action"), t("drepDashboard.governance.votes.vote"), t("drepDashboard.governance.votes.outcome"), t("drepDashboard.governance.votes.type"), t("drepDashboard.governance.votes.rationale")].map((heading, index) => (
                  <Box key={heading} component="th" scope="col" sx={index === 4 ? { ...th, color: "transparent", fontSize: 0 } : th}>{heading}</Box>
                ))}
              </tr>
            </thead>
            <tbody>
              {visible.map(({ row, outcome }) => {
                const id = getFullGovActionId(row.proposal.txHash, row.proposal.index);
                return (
                  <tr key={id} data-testid="drep-vote-row">
                    <Box component="td" sx={{ ...td, maxWidth: 420 }}>
                      <Box component={RouterLink} sx={{ borderBottom: `1px dashed ${ds.gray["400"]}`, color: ds.gray["900"], display: "inline-block", fontWeight: 500, maxWidth: "100%", overflow: "hidden", textDecoration: "none", textOverflow: "ellipsis", verticalAlign: "bottom", whiteSpace: "nowrap", "&:hover": { color: ds.primary["600"] } }} to={getGovActionHistoryPath(row.proposal.txHash, row.proposal.index)}>
                        {row.proposal.title || t("govActions.card.untitled")}
                      </Box>
                      <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>
                        {[timeAgo(row.vote?.date), row.vote ? t("drepDashboard.governance.votes.votedOn", { epoch: row.vote.epochNo }) : ""].filter(Boolean).join(" • ")}
                      </Box>
                    </Box>
                    <Box component="td" sx={td}>{row.vote && <VoteChip vote={row.vote.vote} />}</Box>
                    <Box component="td" sx={td}><OutcomePill outcome={outcome} /></Box>
                    <Box component="td" sx={td}><TypeChip type={row.proposal.type} /></Box>
                    <Box component="td" sx={{ ...td, textAlign: "right", whiteSpace: "nowrap" }}>
                      {row.vote?.url ? (
                        <ButtonBase data-testid="drep-vote-view-rationale" onClick={() => setOpen(row)} sx={{ borderRadius: "4px", color: ds.primary["600"], fontSize: 14, fontWeight: 600, gap: 0.5, "&:hover": { textDecoration: "underline" }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}>
                          {t("drepDashboard.governance.votes.viewRationale")}
                          <ArrowRight aria-hidden size={14} />
                        </ButtonBase>
                      ) : (
                        <Box component="span" sx={{ color: ds.gray["500"], fontSize: 13 }}>{t("drepDashboard.governance.votes.noRationale")}</Box>
                      )}
                    </Box>
                  </tr>
                );
              })}
            </tbody>
          </TableFrame>
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
      )}
      {open && <RationaleDialog onClose={() => setOpen(null)} row={open} />}
    </SubPanel>
  );
};

/** Actions whose deposit returns to this wallet, when records say so. */
export const submittedBy = (actions: GovernanceActionRecord[] | undefined, stakeKey?: string) => {
  const list = actions ?? [];
  const isKnown = list.some((record) => "deposit_return_address" in record);
  const mine = new Set([stakeKey?.toLowerCase(), stakeHexToBech32(stakeKey)].filter(Boolean));
  return {
    isKnown,
    rows: list.filter(
      (record) =>
        !!record.deposit_return_address &&
        mine.has(record.deposit_return_address.toLowerCase()),
    ),
  };
};

const SubmittedProposals = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const [search, setSearch] = useState("");
  const [sort, setSort] = useState("newest");
  const [page, setPage] = useState(1);
  const { isKnown, rows } = submittedBy(data.actions, data.stakeKey);
  const term = search.trim().toLowerCase();
  const filtered = rows
    .filter((record) => !term || (record.title ?? "").toLowerCase().includes(term) || getFullGovActionId(record.tx_hash, record.index).toLowerCase().includes(term) || record.id.toLowerCase().includes(term))
    .sort((a, b) => (sort === "oldest" ? 1 : -1) * (new Date(a.time).getTime() - new Date(b.time).getTime()));
  const visible = filtered.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);

  if (data.actionsLoading) return <Skeleton height={320} variant="rounded" />;
  if (data.actionsError) return <ErrorState message={t("drepDashboard.governance.proposals.error")} onRetry={data.retryActions} />;
  if (!isKnown) {
    return <EmptyState body={t("drepDashboard.governance.proposals.unknownBody")} icon={<Landmark size={20} />} testId="drep-proposals-unknown" title={t("drepDashboard.governance.proposals.unknownTitle")} />;
  }

  return (
    <SubPanel
      count={t("drepDashboard.governance.proposals.count", { count: filtered.length })}
      filters={<SearchBox label={t("drepDashboard.governance.proposals.search")} onChange={(next) => { setSearch(next); setPage(1); }} value={search} />}
      sort={(
        <SortSelect
          dataTestId="drep-proposals-sort"
          label={t("drepDashboard.sortBy")}
          onChange={setSort}
          options={[{ key: "newest", label: t("drepDashboard.governance.proposals.newest") }, { key: "oldest", label: t("drepDashboard.governance.proposals.oldest") }]}
          value={sort}
        />
      )}
    >
      {!rows.length && (
        <EmptyState
          actions={(
            <>
              <Button component={RouterLink} size="medium" to={PATHS.createGovernanceAction}>{t("drepDashboard.governance.proposals.submit")}</Button>
              <Button onClick={() => openInNewTab(LINKS.PROPOSE_A_GOVERNANCE_ACTION)} size="medium" variant="outlined">{t("drepDashboard.governance.proposals.learn")}</Button>
            </>
          )}
          body={t("drepDashboard.governance.proposals.emptyBody")}
          icon={<Landmark size={20} />}
          testId="drep-proposals-empty"
          title={t("drepDashboard.governance.proposals.emptyTitle")}
        />
      )}
      {rows.length > 0 && !filtered.length && <Box sx={{ color: ds.gray["600"], fontSize: 14, py: 4, textAlign: "center" }}>{t("drepDashboard.governance.proposals.noMatch")}</Box>}
      {filtered.length > 0 && (
        <>
          <TableFrame label={t("drepDashboard.governance.activity.tabs.proposals")}>
            <thead>
              <tr>
                {[t("drepDashboard.governance.proposals.proposal"), t("drepDashboard.governance.proposals.submitted"), t("drepDashboard.governance.proposals.type"), t("drepDashboard.governance.proposals.outcome"), ""].map((heading, index) => (
                  // eslint-disable-next-line react/no-array-index-key
                  <Box key={index} component="th" scope="col" sx={{ ...th, textTransform: "uppercase" }}>{heading}</Box>
                ))}
              </tr>
            </thead>
            <tbody>
              {visible.map((record) => {
                const id = getFullGovActionId(record.tx_hash, record.index);
                return (
                  <tr key={id} data-testid="drep-proposal-row">
                    <Box component="td" sx={{ ...td, fontWeight: 500 }}>{record.title || t("govActions.card.untitled")}</Box>
                    <Box component="td" sx={{ ...td, whiteSpace: "nowrap" }}>{formatShortDate(record.time)}</Box>
                    <Box component="td" sx={td}><TypeChip type={record.type} /></Box>
                    <Box component="td" sx={td}><OutcomePill outcome={actionOutcome(record)} /></Box>
                    <Box component="td" sx={{ ...td, textAlign: "right" }}>
                      <Box component={RouterLink} sx={{ color: ds.primary["600"], fontWeight: 600, textDecoration: "none", whiteSpace: "nowrap" }} to={getGovActionHistoryPath(record.tx_hash, record.index)}>
                        {t("drepDashboard.governance.proposals.view")} →
                      </Box>
                    </Box>
                  </tr>
                );
              })}
            </tbody>
          </TableFrame>
          {filtered.length > PAGE_SIZE && (
            <PaginationFooter
              onPageChange={setPage}
              onPageSizeChange={() => {}}
              page={page}
              pageSize={PAGE_SIZE}
              total={filtered.length}
            />
          )}
        </>
      )}
    </SubPanel>
  );
};

const Delegators = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const [page, setPage] = useState(1);
  const {
    isServed, data: result, isLoading, isError, refetch,
  } = useDRepDelegators(data.dRepID, page, PAGE_SIZE);
  const { addSuccessAlert } = useSnackbar();

  if (!isServed) {
    return <EmptyState body={t("drepDashboard.governance.delegators.unavailableBody")} icon={<Users size={20} />} testId="drep-delegators-unavailable" title={t("drepDashboard.governance.delegators.unavailableTitle")} />;
  }
  if (isLoading) return <Skeleton height={320} variant="rounded" />;
  if (isError) return <ErrorState message={t("drepDashboard.governance.delegators.error")} onRetry={() => { refetch(); }} />;
  const rows = result?.elements ?? [];
  const total = result?.total ?? rows.length;
  if (!rows.length) {
    return (
      <EmptyState
        actions={data.profileLink ? (
          <Button
            onClick={async () => {
              await navigator.clipboard?.writeText(data.profileLink ?? "");
              addSuccessAlert(t("alerts.copiedToClipboard"));
            }}
            size="medium"
            variant="outlined"
          >
            {t("drepDashboard.governance.delegators.share")}
          </Button>
        ) : undefined}
        body={t("drepDashboard.governance.delegators.emptyBody")}
        icon={<Users size={20} />}
        testId="drep-delegators-empty"
        title={t("drepDashboard.governance.delegators.emptyTitle")}
      />
    );
  }
  return (
    <SubPanel count={t("drepDashboard.governance.delegators.showing", { count: total })}>
      <TableFrame label={t("drepDashboard.governance.activity.tabs.delegators")} minWidth={640}>
        <thead>
          <tr>
            {[t("drepDashboard.governance.delegators.rank"), t("drepDashboard.governance.delegators.delegator"), t("drepDashboard.governance.delegators.votingPower"), t("drepDashboard.governance.delegators.since")].map((heading, index) => (
              <Box key={heading} component="th" scope="col" sx={{ ...th, textAlign: index === 2 ? "right" : "left", textTransform: "uppercase" }}>{heading}</Box>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((row, index) => (
            <tr key={row.stakeAddress} data-testid="drep-delegator-row">
              <Box component="td" sx={{ ...td, color: ds.gray["500"], width: 40 }}>{(page - 1) * PAGE_SIZE + index + 1}</Box>
              <Box component="td" sx={td}>
                <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
                  <Initials name={row.stakeAddress.slice(-4)} size={28} />
                  <Box component="span" sx={{ fontFamily: "monospace", fontSize: 13 }} title={row.stakeAddress}>{shortId(row.stakeAddress, 12, 6)}</Box>
                  <CopyButton label={t("drepDashboard.governance.delegators.delegator")} value={row.stakeAddress} />
                </Box>
              </Box>
              <Box component="td" sx={{ ...td, fontWeight: 600, textAlign: "right" }}>{formatAda(row.votingPower, 0)}</Box>
              <Box component="td" sx={{ ...td, color: ds.gray["600"] }}>{formatShortDate(row.delegatedAt?.time)}</Box>
            </tr>
          ))}
        </tbody>
      </TableFrame>
      {total > PAGE_SIZE && (
        <PaginationFooter
          onPageChange={setPage}
          onPageSizeChange={() => {}}
          page={page}
          pageSize={PAGE_SIZE}
          total={total}
        />
      )}
    </SubPanel>
  );
};

type DelegationRowData = {
  key: string;
  type: "drep" | "abstain" | "noConfidence";
  dRepId: string | null;
  power: number | null;
  date: string | null;
  isCurrent: boolean;
};

export const delegationTarget = (view: string | null | undefined): DelegationRowData["type"] => {
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_abstain) return "abstain";
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence) return "noConfidence";
  return "drep";
};

const DelegationName = ({ row, ownId }: { row: DelegationRowData; ownId: string }) => {
  const { t } = useTranslation();
  const { dRep } = useGetDRepDetailsQuery(row.type === "drep" ? row.dRepId : undefined, { enabled: row.type === "drep" && !!row.dRepId && row.isCurrent });
  if (row.type !== "drep") return <>{t(`drepDashboard.governance.delegations.${row.type}`)}</>;
  if (row.dRepId && (row.dRepId === ownId || dRep?.drepId === ownId)) return <>{t("drepDashboard.governance.delegations.yourself")}</>;
  return <>{dRep?.givenName || shortId(row.dRepId ?? "", 10, 6)}</>;
};

const MyDelegations = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const { currentDelegation } = useGetAdaHolderCurrentDelegationQuery(data.stakeKey);
  const { votingPower } = useGetAdaHolderVotingPowerQuery(data.stakeKey);
  const history = useDelegationHistory(data.stakeKey);

  const rows: DelegationRowData[] = history.isServed && history.data
    ? history.data.elements.map((event, index) => ({
      key: `${event.txHash}-${index}`,
      type: event.target.type,
      dRepId: event.target.dRepId,
      power: index === 0 ? votingPower ?? null : null,
      date: event.at.time,
      isCurrent: index === 0,
    }))
    : currentDelegation?.dRepView
      ? [{
        key: "current",
        type: delegationTarget(currentDelegation.dRepView),
        dRepId: currentDelegation.dRepView,
        power: votingPower ?? null,
        date: null,
        isCurrent: true,
      }]
      : [];

  if (history.isServed && history.isLoading) return <Skeleton height={240} variant="rounded" />;
  if (history.isServed && history.isError) return <ErrorState message={t("drepDashboard.governance.delegations.error")} onRetry={() => { history.refetch(); }} />;
  if (!rows.length) {
    return (
      <EmptyState
        actions={(
          <>
            <Button component={RouterLink} size="medium" to={PATHS.dRepDirectory}>{t("drepDashboard.governance.delegations.find")}</Button>
            <Button component={RouterLink} size="medium" to={PATHS.registerAsDirectVoter} variant="outlined">{t("drepDashboard.governance.delegations.direct")}</Button>
          </>
        )}
        body={t("drepDashboard.governance.delegations.emptyBody")}
        icon={<Send size={20} />}
        testId="drep-delegations-empty"
        title={t("drepDashboard.governance.delegations.emptyTitle")}
      />
    );
  }
  return (
    <SubPanel count={t("drepDashboard.governance.delegations.showing", { count: rows.length })}>
      <TableFrame label={t("drepDashboard.governance.activity.tabs.delegations")} minWidth={640}>
        <thead>
          <tr>
            {[t("drepDashboard.governance.delegations.drep"), t("drepDashboard.governance.delegations.power"), t("drepDashboard.governance.delegations.since"), ""].map((heading, index) => (
              // eslint-disable-next-line react/no-array-index-key
              <Box key={index} component="th" scope="col" sx={{ ...th, textAlign: index === 1 ? "right" : "left", textTransform: "uppercase" }}>{heading}</Box>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={row.key} data-testid="drep-delegation-row">
              <Box component="td" sx={td}>
                <Box sx={{ alignItems: "center", display: "flex", gap: 1 }}>
                  <Initials name={row.dRepId ?? row.type} size={28} />
                  <Box component="span" sx={{ fontWeight: 500 }}><DelegationName ownId={data.dRepID} row={row} /></Box>
                  {row.isCurrent && <Pill label={t("drepDashboard.governance.delegations.current")} tone="primary" />}
                </Box>
              </Box>
              <Box component="td" sx={{ ...td, textAlign: "right" }}>{row.power === null ? "–" : formatAda(row.power, 0)}</Box>
              <Box component="td" sx={{ ...td, color: ds.gray["600"] }}>{row.date ? formatShortDate(row.date) : "–"}</Box>
              <Box component="td" sx={{ ...td, textAlign: "right" }}>
                {row.type === "drep" && row.dRepId && (
                  <Box component={RouterLink} sx={{ color: ds.primary["600"], fontWeight: 600, textDecoration: "none", whiteSpace: "nowrap" }} to={PATHS.dRepDirectoryDRep.replace(":dRepId", encodeURIComponent(row.dRepId))}>
                    {t("drepDashboard.governance.delegations.viewProfile")} →
                  </Box>
                )}
              </Box>
            </tr>
          ))}
        </tbody>
      </TableFrame>
      {!history.isServed && <Box sx={{ color: ds.gray["500"], fontSize: 13 }}>{t("drepDashboard.governance.delegations.historyNote")}</Box>}
    </SubPanel>
  );
};

const SearchBox = ({
  value,
  onChange,
  label,
}: {
  value: string;
  onChange: (next: string) => void;
  label: string;
}) => (
  <Box sx={{ alignItems: "center", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.md}px`, display: "flex", gap: 1, height: 36, minWidth: { xxs: "100%", sm: 280 }, px: 1.5, "&:focus-within": { borderColor: ds.primary["600"] } }}>
    <Search aria-hidden color={ds.gray["500"]} size={16} />
    <Box aria-label={label} component="input" onChange={(event) => onChange((event.target as HTMLInputElement).value)} placeholder={label} sx={{ border: "none", color: ds.gray["900"], flex: 1, fontSize: 14, outline: "none" }} type="search" value={value} />
  </Box>
);

/** A sub-tab's filter row above its content. */
const SubPanel = ({
  filters,
  count,
  sort,
  children,
}: {
  filters?: ReactNode;
  count?: string;
  sort?: ReactNode;
  children: ReactNode;
}) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
    {(filters || count || sort) && (
      <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
        <Box sx={{ display: "flex", flexWrap: "wrap", gap: 1.5 }}>{filters}</Box>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5 }}>
          {count && <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{count}</Box>}
          {sort}
        </Box>
      </Box>
    )}
    {children}
  </Box>
);

export const ErrorState = ({ message, onRetry }: { message: string; onRetry: () => void }) => {
  const { t } = useTranslation();
  return (
    <EmptyState
      actions={<Button onClick={onRetry} size="medium">{t("drepDashboard.retry")}</Button>}
      icon={<CircleX size={20} />}
      title={message}
      tone="error"
    />
  );
};

const votesCsv = (data: DashboardData) =>
  toCsv([
    ["Governance action", "Action ID", "Type", "Vote", "Epoch", "Voted at", "Outcome", "Rationale URL"],
    ...sortedVotes(data).filter((row) => row.vote).map((row) => [
      row.proposal.title ?? "",
      getFullGovActionId(row.proposal.txHash, row.proposal.index),
      row.proposal.type,
      row.vote?.vote ?? "",
      row.vote?.epochNo ?? "",
      row.vote?.date ?? "",
      actionOutcome(data.actionsByKey.get(actionKey(row.proposal.txHash, row.proposal.index))),
      row.vote?.url ?? "",
    ]),
  ]);

const proposalsCsv = (data: DashboardData) =>
  toCsv([
    ["Proposal", "Action ID", "Type", "Submitted", "Outcome"],
    ...submittedBy(data.actions, data.stakeKey).rows.map((record) => [
      record.title ?? "",
      getFullGovActionId(record.tx_hash, record.index),
      record.type,
      record.time,
      actionOutcome(record),
    ]),
  ]);

/** The activity card: four tables behind underlined tabs. */
const ActivityCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const [tab, setTab] = useState<SubTab>("votes");
  const canExport = tab === "votes" || tab === "proposals";
  return (
    <Panel
      action={(
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5 }}>
          {data.updatedAt > 0 && (
            <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("drepDashboard.lastUpdated", { ago: timeAgo(data.updatedAt) })}</Box>
          )}
          <ExportButton
            disabled={!canExport}
            onClick={() => downloadCsv(`govtool-drep-${tab}-${today()}.csv`, tab === "votes" ? votesCsv(data) : proposalsCsv(data))}
          />
        </Box>
      )}
      subtitle={t(`drepDashboard.governance.activity.subtitles.${tab}`)}
      testId="drep-activity-card"
      title={t("drepDashboard.governance.activity.title")}
      titleId="drep-activity-title"
    >
      <UnderlineTabs
        idPrefix="drep-activity"
        label={t("drepDashboard.governance.activity.tabsLabel")}
        onChange={(next) => setTab(next as SubTab)}
        tabs={SUB_TABS.map((value) => ({ value, label: t(`drepDashboard.governance.activity.tabs.${value}`) }))}
        value={tab}
      />
      <Box aria-labelledby={`drep-activity-tab-${tab}`} id="drep-activity-panel" role="tabpanel">
        {tab === "votes" && <MyVotes data={data} />}
        {tab === "proposals" && <SubmittedProposals data={data} />}
        {tab === "delegators" && <Delegators data={data} />}
        {tab === "delegations" && <MyDelegations data={data} />}
      </Box>
    </Panel>
  );
};

export const GovernanceTab = ({ data }: { data: DashboardData }) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5 }}>
    <VotingHistoryCard data={data} />
    <DistributionCard data={data} />
    <ActivityCard data={data} />
  </Box>
);
