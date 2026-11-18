import { useMemo, useState } from "react";
import { Box, ButtonBase, Dialog, Skeleton } from "@mui/material";
import { useTranslation } from "react-i18next";
import { ArrowUpRight, ChevronRight, Copy, ExternalLink, History, TriangleAlert, X } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { PaginationFooter } from "@/components/molecules/PaginationFooter";
import { SortSelect } from "@/components/molecules/SortSelect";
import { ds, RADIUS } from "@/consts/designTokens";
import { useAppContext } from "@/context/appContext";
import { useSnackbar } from "@/context/snackbar";
import { useDRepHistory } from "@/hooks/queries/useDRepDashboard";
import type { DRepHistoryKind, DRepHistoryRow } from "@/models";
import { openInNewTab } from "@/utils/openInNewTab";

import { EmptyState, Pill, safeUrl, shortId, CopyButton } from "./parts";
import type { DashboardData } from "./types";

const PAGE_SIZE = 12;
const KINDS: DRepHistoryKind[] = ["registered", "reregistered", "updated", "retired", "statusChanged"];
const STATUSES = ["active", "inactive", "retired"] as const;
const STATUS_TONES = { active: "success", inactive: "warning", retired: "neutral" } as const;

const day = (time: string) => {
  const date = new Date(time);
  return Number.isNaN(date.getTime())
    ? ""
    : date.toLocaleDateString("en-GB", { day: "2-digit", month: "short", timeZone: "UTC", year: "numeric" });
};

const clock = (time: string) => {
  const date = new Date(time);
  return Number.isNaN(date.getTime())
    ? ""
    : `${date.toLocaleTimeString("en-GB", { hour: "2-digit", hour12: false, minute: "2-digit", timeZone: "UTC" })} UTC`;
};

/**
 * The rows GovTool can show without a history route: the latest
 * registration, from the DRep record.
 */
export const fallbackHistory = (data: DashboardData): DRepHistoryRow[] => {
  const time = data.dRep?.latestRegistrationDate;
  if (!time) return [];
  const status = (data.dRep?.status ?? "Active").toLowerCase();
  return [{
    kind: "registered",
    status: STATUSES.includes(status as typeof STATUSES[number]) ? (status as DRepHistoryRow["status"]) : "active",
    at: { epoch: data.registeredEpoch ?? Number.NaN, time },
    txHash: data.voter.dRepRegisterTxHash,
    anchor: data.voter.url ? { url: data.voter.url, hash: data.dRep?.metadataHash ?? "" } : null,
  }];
};

const subtitle = (row: DRepHistoryRow, t: (key: string) => string) =>
  (row.kind === "statusChanged"
    ? t(`drepDashboard.history.kinds.statusChanged.${row.status}`)
    : t(`drepDashboard.history.kinds.${row.kind}.body`));

const HistoryDialog = ({ row, onClose }: { row: DRepHistoryRow; onClose: () => void }) => {
  const { t } = useTranslation();
  const { cExplorerBaseUrl } = useAppContext();
  const { addSuccessAlert } = useSnackbar();
  return (
    <Dialog aria-labelledby="drep-history-dialog-title" onClose={onClose} open PaperProps={{ sx: { borderRadius: `${RADIUS.xl}px`, maxWidth: 600, width: "100%" } }}>
      <Box data-testid="drep-history-dialog">
        <Box sx={{ alignItems: "center", borderBottom: `1px solid ${ds.gray["200"]}`, display: "flex", justifyContent: "space-between", px: 3, py: 2 }}>
          <Box component="h2" id="drep-history-dialog-title" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0 }}>
            {t(`drepDashboard.history.kinds.${row.kind}.title`)}
          </Box>
          <ButtonBase aria-label={t("drepDashboard.history.details.close")} onClick={onClose} sx={{ backgroundColor: ds.gray["100"], borderRadius: "50%", color: ds.gray["700"], p: 0.5, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}>
            <X aria-hidden size={16} />
          </ButtonBase>
        </Box>
        <Box sx={{ display: "flex", flexDirection: "column", gap: 2, p: 3 }}>
          <Box sx={{ backgroundColor: ds.primary["25"], borderRadius: `${RADIUS.md}px`, p: 2 }}>
            <Box sx={{ color: ds.gray["700"], fontSize: 14 }}>{t(`drepDashboard.history.details.summary.${row.kind}`)}</Box>
            <Box sx={{ color: ds.gray["900"], fontSize: 15, fontWeight: 600, mt: 0.5 }}>{`${day(row.at.time)} · ${clock(row.at.time)}`}</Box>
          </Box>
          {row.txHash ? (
            <Box sx={{ borderBottom: `1px solid ${ds.gray["200"]}`, pb: 2 }}>
              <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("drepDashboard.history.details.tx")}</Box>
              <Box sx={{ alignItems: "center", display: "flex", gap: 0.5, mt: 0.5 }}>
                <Box component="span" data-testid="drep-history-dialog-hash" sx={{ color: ds.gray["900"], fontFamily: "monospace", fontSize: 13, overflowWrap: "anywhere" }}>{row.txHash}</Box>
                <CopyButton label={t("drepDashboard.history.details.tx")} value={row.txHash} />
              </Box>
            </Box>
          ) : (
            <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>{t("drepDashboard.history.details.noTx")}</Box>
          )}
          {Number.isFinite(row.at.epoch) && (
            <Box sx={{ borderBottom: `1px solid ${ds.gray["200"]}`, pb: 2 }}>
              <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t("drepDashboard.history.details.epoch")}</Box>
              <Box sx={{ color: ds.gray["900"], fontSize: 14, mt: 0.5 }}>{row.at.epoch}</Box>
            </Box>
          )}
          {row.txHash && (
            <Box sx={{ display: "grid", gap: 1.5, gridTemplateColumns: "1fr 1fr" }}>
              <Button
                onClick={async () => {
                  await navigator.clipboard?.writeText(row.txHash ?? "");
                  addSuccessAlert(t("alerts.copiedToClipboard"));
                }}
                startIcon={<Copy aria-hidden size={16} />}
                variant="outlined"
              >
                {t("drepDashboard.history.details.copy")}
              </Button>
              <Button onClick={() => openInNewTab(`${cExplorerBaseUrl}/tx/${encodeURIComponent(row.txHash ?? "")}`)} startIcon={<ExternalLink aria-hidden size={16} />}>
                {t("drepDashboard.history.details.view")}
              </Button>
            </Box>
          )}
        </Box>
      </Box>
    </Dialog>
  );
};

export const HistoryLogTab = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const [kind, setKind] = useState("all");
  const [status, setStatus] = useState("all");
  const [sort, setSort] = useState<"desc" | "asc">("desc");
  const [page, setPage] = useState(1);
  const [open, setOpen] = useState<DRepHistoryRow | null>(null);
  const history = useDRepHistory(data.dRepID, 1, 100, "desc");

  const source = history.isServed ? history.data?.elements ?? [] : fallbackHistory(data);
  const rows = useMemo(
    () =>
      source
        .filter((row) => (kind === "all" || row.kind === kind) && (status === "all" || row.status === status))
        .sort((a, b) => (sort === "asc" ? 1 : -1) * (new Date(a.at.time).getTime() - new Date(b.at.time).getTime())),
    [source, kind, status, sort],
  );
  const filters = Number(kind !== "all") + Number(status !== "all");
  const visible = rows.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);
  const isLoading = history.isServed && history.isLoading;
  const isError = history.isServed && history.isError;

  let body;
  if (isError) {
    body = (
      <EmptyState
        actions={<Button onClick={() => { history.refetch(); }} size="medium" startIcon={<History aria-hidden size={16} />}>{t("drepDashboard.retry")}</Button>}
        body={t("drepDashboard.history.errorBody")}
        icon={<TriangleAlert size={20} />}
        testId="drep-history-error"
        title={t("drepDashboard.history.errorTitle")}
        tone="error"
      />
    );
  } else if (!isLoading && !source.length) {
    body = <EmptyState body={t("drepDashboard.history.emptyBody")} icon={<History size={20} />} testId="drep-history-empty" title={t("drepDashboard.history.emptyTitle")} />;
  } else {
    body = (
      <Box sx={{ backgroundColor: "#FFFFFF", border: `1px solid ${ds.gray["200"]}`, borderRadius: `${RADIUS.lg}px`, overflow: "hidden" }}>
        <Box sx={{ alignItems: "center", display: "flex", justifyContent: "space-between", px: 2.5, py: 2 }}>
          <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>
            {isLoading ? t("drepDashboard.history.loading") : t("drepDashboard.history.records", { count: rows.length })}
          </Box>
          <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{t(`drepDashboard.history.order.${sort}`)}</Box>
        </Box>
        <Box sx={{ overflowX: "auto" }}>
          <Box aria-busy={isLoading} aria-label={t("drepDashboard.history.title")} component="table" sx={{ borderCollapse: "collapse", minWidth: 860, width: "100%" }}>
            <thead>
              <tr>
                {["date", "activityColumn", "status", "metadata", "transaction"].map((key) => (
                  <Box key={key} component="th" scope="col" sx={{ backgroundColor: ds.gray["50"], color: ds.gray["900"], fontSize: 13, fontWeight: 600, px: 2.5, py: 1.5, textAlign: "left" }}>
                    {t(`drepDashboard.history.${key}`)}
                  </Box>
                ))}
                <Box aria-hidden component="th" sx={{ backgroundColor: ds.gray["50"], width: 40 }} />
              </tr>
            </thead>
            <tbody>
              {isLoading && Array.from({ length: 6 }, (_, index) => (

                <tr key={index}>
                  {[96, 200, 56, 96, 140, 16].map((width, cell) => (
                    // eslint-disable-next-line react/no-array-index-key
                    <Box key={cell} component="td" sx={{ borderTop: `1px solid ${ds.gray["200"]}`, px: 2.5, py: 2 }}><Skeleton width={width} /></Box>
                  ))}
                </tr>
              ))}
              {!isLoading && !rows.length && (
                <tr><Box colSpan={6} component="td" sx={{ color: ds.gray["600"], fontSize: 14, py: 4, textAlign: "center" }}>{t("drepDashboard.history.noMatch")}</Box></tr>
              )}
              {!isLoading && visible.map((row) => {
                const title = t(`drepDashboard.history.kinds.${row.kind}.title`);
                const metadata = safeUrl(row.anchor?.url);
                return (
                  <Box
                    key={`${row.kind}-${row.at.time}-${row.txHash ?? ""}`}
                    component="tr"
                    data-testid="drep-history-row"
                    onClick={() => setOpen(row)}
                    sx={{ borderTop: `1px solid ${ds.gray["200"]}`, cursor: "pointer", "&:hover": { backgroundColor: ds.primary["25"] } }}
                  >
                    <Box component="td" sx={{ px: 2.5, py: 2, whiteSpace: "nowrap" }}>
                      <Box sx={{ color: ds.gray["900"], fontSize: 14 }}>{day(row.at.time)}</Box>
                      <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{clock(row.at.time)}</Box>
                    </Box>
                    <Box component="td" sx={{ px: 2.5, py: 2 }}>
                      <Box sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600 }}>{title}</Box>
                      <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{subtitle(row, t)}</Box>
                    </Box>
                    <Box component="td" sx={{ px: 2.5, py: 2 }}>
                      <Pill label={t(`drepDashboard.history.statuses.${row.status}`)} tone={STATUS_TONES[row.status]} />
                    </Box>
                    <Box component="td" sx={{ px: 2.5, py: 2 }}>
                      {metadata && (
                        <Box component="a" href={metadata} onClick={(event) => event.stopPropagation()} rel="noopener noreferrer" sx={{ alignItems: "center", color: ds.primary["600"], display: "inline-flex", fontSize: 14, fontWeight: 600, gap: 0.25, textDecoration: "none", whiteSpace: "nowrap", "&:hover": { textDecoration: "underline" } }} target="_blank">
                          {t("drepDashboard.history.viewMetadata")}
                          <ArrowUpRight aria-hidden size={14} />
                        </Box>
                      )}
                    </Box>
                    <Box component="td" sx={{ color: ds.gray["600"], fontFamily: "monospace", fontSize: 13, px: 2.5, py: 2 }}>{row.txHash ? shortId(row.txHash, 12, 4) : "–"}</Box>
                    <Box component="td" sx={{ pr: 2, py: 2 }}>
                      <ButtonBase
                        aria-label={t("drepDashboard.history.details.open", { activity: title, date: day(row.at.time) })}
                        data-testid="drep-history-open"
                        onClick={(event) => { event.stopPropagation(); setOpen(row); }}
                        sx={{ borderRadius: "4px", color: ds.gray["500"], p: 0.5, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
                      >
                        <ChevronRight aria-hidden size={18} />
                      </ButtonBase>
                    </Box>
                  </Box>
                );
              })}
            </tbody>
          </Box>
        </Box>
        {!isLoading && rows.length > PAGE_SIZE && (
          <Box sx={{ borderTop: `1px solid ${ds.gray["200"]}`, px: 2.5, py: 1.5 }}>
            <PaginationFooter
              onPageChange={setPage}
              onPageSizeChange={() => {}}
              page={page}
              pageSize={PAGE_SIZE}
              total={rows.length}
            />
          </Box>
        )}
      </Box>
    );
  }

  return (
    <Box data-testid="drep-history" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <Box>
        <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 20, fontWeight: 600, m: 0 }}>{t("drepDashboard.history.title")}</Box>
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, m: 0, mt: 0.5 }}>{t("drepDashboard.history.subtitle")}</Box>
      </Box>
      <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5, justifyContent: "space-between" }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1.5 }}>
          <SortSelect
            dataTestId="drep-history-kind"
            label={t("drepDashboard.history.activity")}
            onChange={(next) => { setKind(next); setPage(1); }}
            options={[{ key: "all", label: t("drepDashboard.history.allActivities") }, ...KINDS.map((key) => ({ key, label: t(`drepDashboard.history.kinds.${key}.title`) }))]}
            value={kind}
          />
          <SortSelect
            dataTestId="drep-history-status"
            label={t("drepDashboard.status")}
            onChange={(next) => { setStatus(next); setPage(1); }}
            options={[{ key: "all", label: t("drepDashboard.all") }, ...STATUSES.map((key) => ({ key, label: t(`drepDashboard.history.statuses.${key}`) }))]}
            value={status}
          />
          <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>
            {filters ? t("drepDashboard.history.filters", { count: filters }) : t("drepDashboard.history.noFilters")}
          </Box>
        </Box>
        <SortSelect
          dataTestId="drep-history-sort"
          label={t("drepDashboard.history.sort")}
          onChange={(next) => setSort(next === "asc" ? "asc" : "desc")}
          options={[{ key: "desc", label: t("drepDashboard.history.newest") }, { key: "asc", label: t("drepDashboard.history.oldest") }]}
          value={sort}
        />
      </Box>
      {body}
      {!history.isServed && source.length > 0 && (
        <Box data-testid="drep-history-partial" sx={{ color: ds.gray["500"], fontSize: 13 }}>{t("drepDashboard.history.partial")}</Box>
      )}
      {open && <HistoryDialog onClose={() => setOpen(null)} row={open} />}
    </Box>
  );
};
