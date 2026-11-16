import { useEffect, useState } from "react";
import { Box, ButtonBase, Skeleton } from "@mui/material";
import { ArrowLeft, ArrowRight } from "lucide-react";

import { PATHS } from "@consts";
import {
  useDebounce,
  useGetLiveProposalsQuery,
  useGetProposalsPageQuery,
  useTranslation,
} from "@hooks";
import type { ProposalData } from "@models";
import {
  getFullGovActionId,
  getGovernanceActionCIP129Id,
  getProposalTypeLabel,
} from "@utils";
import { SortSelect } from "@/components/molecules/SortSelect";
import { SearchField } from "@/components/molecules/SearchField";
import { ds, RADIUS } from "@/consts/designTokens";
import { clampPage } from "@/utils/clampPage";
import { daysLeft, formatUtc } from "@/utils/governanceActivity";

import {
  ActivityBand,
  ActivityCard,
  Badge,
  Category,
  LabelledValue,
  SectionHeading,
  TextLink,
} from "./activityParts";
import { shortActionId } from "./RecentlyRatified";

const PAGE_SIZE = 5;

const chipSx = (isActive: boolean) => ({
  alignItems: "center",
  backgroundColor: isActive ? ds.primary["50"] : "#FFFFFF",
  border: `1px solid ${isActive ? ds.primary["600"] : ds.gray["200"]}`,
  borderRadius: `${RADIUS.pill}px`,
  color: isActive ? ds.primary["700"] : ds.gray["500"],
  fontFamily: "inherit",
  fontSize: 13,
  fontWeight: 500,
  gap: 0.5,
  lineHeight: "19.5px",
  px: 1.5,
  py: 0.75,
  whiteSpace: "nowrap",
  "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
});

const ActionRow = ({ proposal }: { proposal: ProposalData }) => {
  const { t } = useTranslation();
  const typeLabel = getProposalTypeLabel(proposal.type);
  const title = proposal.title || typeLabel;
  const left = daysLeft(proposal.expiryDate, new Date());

  return (
    <Box
      component="li"
      data-testid="active-action-row"
      sx={{
        alignItems: { md: "center" },
        borderTop: `1px solid ${ds.gray["200"]}`,
        display: "grid",
        gap: { xxs: 1.5, md: 2.25 },
        gridTemplateColumns: { xxs: "1fr 1fr", md: "minmax(0, 1fr) 130px 205px 90px" },
        py: 2.5,
      }}
    >
      <Box sx={{ display: "flex", flexDirection: "column", gap: 0.875, gridColumn: { xxs: "1 / -1", md: "auto" }, minWidth: 0 }}>
        <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
          <Badge tone="primary">{t("activity.active.voting")}</Badge>
          <Category>{typeLabel}</Category>
        </Box>
        <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 14, fontWeight: 600, letterSpacing: "-0.09px", lineHeight: "20px", m: 0, overflowWrap: "anywhere" }}>
          {title}
        </Box>
        <Box component="span" sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>
          {shortActionId(
            getGovernanceActionCIP129Id({ tx_hash: proposal.txHash, index: proposal.index }),
          )}
        </Box>
      </Box>
      <LabelledValue label={t("activity.active.submitted")}>
        <Box component="time" dateTime={proposal.createdDate} sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 500, lineHeight: "19.5px" }}>
          {formatUtc(proposal.createdDate, "date")}
        </Box>
      </LabelledValue>
      <LabelledValue label={t("activity.active.deadline")} sx={{ gap: 0.75 }}>
        <Box component="time" dateTime={proposal.expiryDate} sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 600, lineHeight: "19.5px" }}>
          {formatUtc(proposal.expiryDate, "dateTime")}
        </Box>
        {left && (
          <Box sx={{ display: "flex" }}>
            <Badge size="md" tone={left.tone}>
              {left.days > 0
                ? t("activity.active.daysLeft", { count: left.days })
                : t("activity.active.lessThanDay")}
            </Badge>
          </Box>
        )}
      </LabelledValue>
      <Box sx={{ display: "flex", gridColumn: { xxs: "1 / -1", md: "auto" }, justifyContent: { md: "flex-end" } }}>
        <TextLink
          dataTestId={`activity-${getFullGovActionId(proposal.txHash, proposal.index)}-details`}
          label={`${t("activity.active.details")}: ${title}`}
          state={{ proposal }}
          to={`${PATHS.governanceActions}/${getFullGovActionId(proposal.txHash, proposal.index)}`}
        >
          {t("activity.active.details")}
        </TextLink>
      </Box>
    </Box>
  );
};

/** The live actions with search, a type filter, sorting and paging. */
export const ActiveActionsList = () => {
  const { t } = useTranslation();
  const [search, setSearch] = useState("");
  const [type, setType] = useState("");
  const [sorting, setSorting] = useState("SoonestToExpire");
  const searchPhrase = useDebounce(search.trim(), 400);
  // The page belongs to the filters it was chosen under; new filters start
  // on page 1 in the same render, so no request goes out for the old page.
  const filterKey = `${searchPhrase}|${type}|${sorting}`;
  const [pageState, setPageState] = useState({ key: filterKey, page: 1 });
  const page = pageState.key === filterKey ? pageState.page : 1;
  const setPage = (next: number) => setPageState({ key: filterKey, page: next });

  const { liveProposals, liveProposalsTotal } = useGetLiveProposalsQuery();
  const { proposals, proposalsTotal, isProposalsLoading, isSearchNotReady } =
    useGetProposalsPageQuery({
      filters: type ? [type] : [],
      page,
      pageSize: PAGE_SIZE,
      searchPhrase,
      sorting,
    });

  // A filter that shrinks the list must not leave the page past its end.
  useEffect(() => {
    if (isProposalsLoading) return;
    const clamped = clampPage(page, proposalsTotal, PAGE_SIZE);
    if (clamped !== page) setPage(clamped);
  }, [page, proposalsTotal, isProposalsLoading]);

  // Counts come from the cached live list and show only when it is whole.
  const isListWhole = liveProposalsTotal <= liveProposals.length;
  const typeCounts = liveProposals.reduce<Record<string, number>>((counts, proposal) => {
    counts[proposal.type] = (counts[proposal.type] ?? 0) + 1;
    return counts;
  }, {});
  const types = Object.keys(typeCounts).sort((a, b) => typeCounts[b] - typeCounts[a]);
  if (type && !types.includes(type)) types.push(type);

  const from = proposalsTotal ? (page - 1) * PAGE_SIZE + 1 : 0;
  const to = Math.min(page * PAGE_SIZE, proposalsTotal);
  const lastPage = Math.max(1, Math.ceil(proposalsTotal / PAGE_SIZE));

  const pagerSx = (isEnabled: boolean) => ({
    borderRadius: `${RADIUS.xs}px`,
    color: isEnabled ? ds.primary["600"] : ds.gray["400"],
    fontFamily: "inherit",
    fontSize: 14,
    fontWeight: 600,
    gap: 0.75,
    lineHeight: "20px",
    "&.Mui-focusVisible": { outline: `2px solid ${ds.primary["600"]}`, outlineOffset: 2 },
  });

  return (
    <ActivityBand
      background="radial-gradient(50% 70% at 100% 100%, #FFF0E287 0%, #FFF0E200 100%), radial-gradient(50% 70% at 0% 0%, #E7EFFC87 0%, #E7EFFC00 100%), #F7F8F6"
      id="active-actions"
      labelledBy="active-actions-heading"
      sx={{ display: "flex", flexDirection: "column", gap: 3.25 }}
    >
      <SectionHeading
        description={t("activity.active.description")}
        eyebrow={t("activity.active.eyebrow")}
        id="active-actions-heading"
        link={
          <TextLink dataTestId="activity-explore-all-link" to={PATHS.governanceActions}>
            {t("activity.active.explore")}
          </TextLink>
        }
        title={t("activity.active.title")}
      />
      <Box
        sx={{
          alignItems: { lg: "center" },
          display: "flex",
          flexDirection: { xxs: "column", lg: "row" },
          gap: 2,
          justifyContent: "space-between",
        }}
      >
        <Box sx={{ display: "flex", maxWidth: { lg: 410 }, width: "100%" }}>
          <SearchField
            onChange={setSearch}
            onClear={() => setSearch("")}
            placeholder={t("activity.active.search")}
            value={search}
          />
        </Box>
        <Box
          aria-label={t("activity.active.filtersLabel")}
          role="group"
          sx={{ display: "flex", flexWrap: "wrap", gap: 1 }}
        >
          <ButtonBase
            aria-pressed={type === ""}
            data-testid="activity-type-all"
            onClick={() => setType("")}
            sx={chipSx(type === "")}
          >
            {t("activity.active.all")}
            {isListWhole && (
              <Box component="span" sx={{ fontSize: 12, fontWeight: 600 }}>
                {liveProposalsTotal}
              </Box>
            )}
          </ButtonBase>
          {types.map((key) => (
            <ButtonBase
              key={key}
              aria-pressed={type === key}
              data-testid={`activity-type-${key}`}
              onClick={() => setType(type === key ? "" : key)}
              sx={chipSx(type === key)}
            >
              {getProposalTypeLabel(key)}
              {isListWhole && typeCounts[key] !== undefined && (
                <Box component="span" sx={{ fontSize: 12, fontWeight: 600 }}>
                  {typeCounts[key]}
                </Box>
              )}
            </ButtonBase>
          ))}
        </Box>
      </Box>
      <ActivityCard dataTestId="active-actions-list">
        <Box
          sx={{
            alignItems: "center",
            borderBottom: `1px solid ${ds.gray["200"]}`,
            display: "flex",
            flexWrap: "wrap",
            gap: 1.5,
            justifyContent: "space-between",
            pb: 1.5,
            pt: 2,
          }}
        >
          <Box aria-live="polite" component="p" data-testid="active-actions-count" sx={{ color: ds.gray["900"], fontSize: 13, fontWeight: 600, lineHeight: "19.5px", m: 0 }}>
            {isProposalsLoading ? "—" : t("activity.active.count", { count: proposalsTotal })}
          </Box>
          <SortSelect
            dataTestId="activity-sort-button"
            onChange={setSorting}
            options={[
              { key: "SoonestToExpire", label: t("activity.active.sortSoonest") },
              { key: "NewestCreated", label: t("activity.active.sortNewest") },
            ]}
            value={sorting}
          />
        </Box>
        {isProposalsLoading && (
          <Box aria-hidden sx={{ display: "flex", flexDirection: "column", gap: 2, py: 2.5 }}>
            {[0, 1, 2].map((key) => (
              <Skeleton key={key} height={72} sx={{ borderRadius: `${RADIUS.md}px` }} variant="rectangular" />
            ))}
          </Box>
        )}
        {!isProposalsLoading && !proposals?.length && (
          <Box component="p" sx={{ color: ds.gray["500"], fontSize: 14, m: 0, py: 4, textAlign: "center" }}>
            {isSearchNotReady ? t("activity.active.searchNotReady") : t("activity.active.empty")}
          </Box>
        )}
        {!isProposalsLoading && !!proposals?.length && (
          <Box component="ul" sx={{ listStyle: "none", m: 0, p: 0, "& > li:first-of-type": { borderTop: "none" } }}>
            {proposals.map((proposal) => (
              <ActionRow key={`${proposal.txHash}#${proposal.index}`} proposal={proposal} />
            ))}
          </Box>
        )}
        {proposalsTotal > 0 && (
          <Box
            component="nav"
            aria-label={t("pagination.label")}
            sx={{
              alignItems: "center",
              borderTop: `1px solid ${ds.gray["200"]}`,
              display: "flex",
              flexWrap: "wrap",
              gap: 1.5,
              justifyContent: "space-between",
              py: 2,
            }}
          >
            <Box component="span" sx={{ color: ds.gray["500"], fontSize: 13, lineHeight: "19.5px" }}>
              {t("activity.active.showing", { from, to, total: proposalsTotal })}
            </Box>
            <Box sx={{ display: "flex", gap: 3 }}>
              <ButtonBase
                data-testid="activity-previous-page"
                disabled={page <= 1}
                onClick={() => setPage(page - 1)}
                sx={pagerSx(page > 1)}
              >
                <ArrowLeft aria-hidden size={16} />
                {t("activity.active.previous")}
              </ButtonBase>
              <ButtonBase
                data-testid="activity-next-page"
                disabled={page >= lastPage}
                onClick={() => setPage(page + 1)}
                sx={pagerSx(page < lastPage)}
              >
                {t("activity.active.next")}
                <ArrowRight aria-hidden size={16} />
              </ButtonBase>
            </Box>
          </Box>
        )}
      </ActivityCard>
    </ActivityBand>
  );
};
