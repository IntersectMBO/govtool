import { ReactNode } from "react";
import { Box, Skeleton } from "@mui/material";
import { Link as RouterLink } from "react-router";
import { useTranslation } from "react-i18next";
import { ArrowRight, Archive, PenLine } from "lucide-react";

import { Button } from "@/components/atoms/Button";
import { GOV_ACTION_HISTORY_PATHS, PATHS } from "@/consts/paths";
import { ds, RADIUS, SHADOWS } from "@/consts/designTokens";
import { useDRepDelegators } from "@/hooks/queries/useDRepDashboard";
import { getFullGovActionId } from "@/utils/getGovActionId";
import { getProposalTypeLabel } from "@/utils/getProposalTypeLabel";
import { actionKey, actionOutcome } from "@/utils/drepDashboard";

import {
  ArrowLink,
  CopyButton,
  EmptyState,
  ExternalText,
  formatAda,
  formatShortDate,
  Initials,
  Panel,
  Pill,
  safeUrl,
  shortId,
  TableFrame,
  td,
  th,
  VOTE_COLORS,
  VOTE_TEXT,
} from "./parts";
import type { DashboardData } from "./types";

const STATUS_TONES = { Active: "primary", Inactive: "neutral", Retired: "warning", Yourself: "primary" } as const;

const Label = ({ children }: { children: ReactNode }) => (
  <Box sx={{ color: ds.gray["600"], fontSize: 13, lineHeight: "20px" }}>{children}</Box>
);

const Divider = () => <Box aria-hidden sx={{ borderTop: `1px solid ${ds.gray["200"]}` }} />;

/** The DRep's name, status, voting power and delegators. */
export const RepresentationCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const { voter, dRep, dRepID } = data;
  const delegators = useDRepDelegators(dRepID, 1, 1);
  const name = voter.givenName || dRep?.givenName || "";
  const power = voter.votingPower ?? dRep?.votingPower ?? 0;
  const total = delegators.isServed ? delegators.data?.total : undefined;
  return (
    <Box
      component="section"
      data-testid="drep-representation-card"
      sx={{
        background: `linear-gradient(180deg, ${ds.orange["50"]} 0%, #FFFFFF 45%)`,
        border: `1px solid ${ds.gray["200"]}`,
        borderRadius: `${RADIUS.lg}px`,
        boxShadow: SHADOWS.card,
        display: "flex",
        flexDirection: "column",
        gap: 2,
        p: { xxs: 2, md: 3 },
      }}
    >
      <Box sx={{ alignItems: "center", display: "flex", gap: 1.5, minWidth: 0 }}>
        {safeUrl(dRep?.imageUrl) ? (
          <Box alt="" component="img" src={safeUrl(dRep?.imageUrl)} sx={{ borderRadius: "50%", flexShrink: 0, height: 48, objectFit: "cover", width: 48 }} />
        ) : (
          <Initials name={name || dRep?.view || "DRep"} size={48} />
        )}
        <Box sx={{ minWidth: 0 }}>
          <Box sx={{ alignItems: "center", display: "flex", flexWrap: "wrap", gap: 1 }}>
            <Box component="h2" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0, overflowWrap: "anywhere" }}>
              {name || t("drepDashboard.overview.representation.title")}
            </Box>
            {dRep?.status && <Pill label={t(`drepDashboard.history.statuses.${dRep.status.toLowerCase()}`, dRep.status)} tone={STATUS_TONES[dRep.status]} />}
          </Box>
          {dRep?.view && (
            <Box sx={{ color: ds.gray["600"], fontFamily: "monospace", fontSize: 13 }}>{shortId(dRep.view, 9, 4)}</Box>
          )}
        </Box>
      </Box>
      <Divider />
      <Box>
        <Box sx={{ color: ds.gray["600"], fontSize: 12, fontWeight: 600, letterSpacing: "0.4px", textTransform: "uppercase" }}>
          {t("drepDashboard.overview.representation.title")}
        </Box>
        <Box data-testid="drep-representation-voting-power" sx={{ color: ds.gray["900"], fontSize: 30, fontWeight: 600, letterSpacing: "-0.6px", mt: 1.5 }}>
          ₳ {formatAda(power, 0)}
        </Box>
        <Label>{t("drepDashboard.overview.representation.votingPower")}</Label>
      </Box>
      {total !== undefined && (
        <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>
          {t("drepDashboard.overview.representation.delegators")}{" "}
          <Box component="span" sx={{ color: ds.gray["900"], fontWeight: 600 }}>{total.toLocaleString("en-US")}</Box>
        </Box>
      )}
      {!power && (
        <Box component="p" sx={{ borderTop: `1px dashed ${ds.gray["200"]}`, color: ds.gray["600"], fontSize: 13, lineHeight: "20px", m: 0, pt: 1.5 }}>
          {t("drepDashboard.overview.representation.note")}
        </Box>
      )}
      {dRep?.view && (
        <ArrowLink testId="view-drep-details-button" to={PATHS.dashboardDRepDirectoryDRep.replace(":dRepId", encodeURIComponent(dRep.view))}>
          {t("drepDashboard.overview.representation.viewProfile")}
        </ArrowLink>
      )}
    </Box>
  );
};

const Field = ({ label, children }: { label: string; children: ReactNode }) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 0.25, minWidth: 0 }}>
    <Label>{label}</Label>
    {children}
  </Box>
);

const IdRow = ({ value, label, testId }: { value: string; label: string; testId?: string }) => (
  <Box data-testid={testId} sx={{ alignItems: "center", display: "flex", gap: 0.5, minWidth: 0 }}>
    <Box component="span" sx={{ color: ds.primary["600"], fontSize: 14, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }} title={value}>
      {value}
    </Box>
    <CopyButton label={label} value={value} />
  </Box>
);

/** DRep id, payment address, metadata and identity links. */
const IdentityCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const { voter, dRep } = data;
  const links = dRep?.identityReferences ?? [];
  return (
    <Panel
      action={!links.length ? <ArrowLink to={PATHS.editDrepMetadata}>{t("drepDashboard.overview.identity.addLinks")}</ArrowLink> : undefined}
      testId="drep-identity-card"
      title={t("drepDashboard.overview.identity.title")}
      titleId="drep-identity-title"
    >
      {dRep?.view && (
        <Field label={t("drepDashboard.overview.identity.drepId")}>
          <IdRow label={t("drepDashboard.overview.identity.drepId")} testId="dRep-id-display-card-dashboard" value={dRep.view} />
        </Field>
      )}
      {voter.paymentAddress && (
        <Field label={t("drepDashboard.overview.identity.paymentAddress")}>
          <IdRow label={t("drepDashboard.overview.identity.paymentAddress")} value={voter.paymentAddress} />
          <Box sx={{ color: ds.gray["500"], fontSize: 12, lineHeight: "18px" }}>{t("drepDashboard.overview.identity.paymentHint")}</Box>
        </Field>
      )}
      {voter.url && (
        <Field label={t("drepDashboard.overview.identity.metadataUrl")}>
          <ExternalText href={voter.url}>{voter.url}</ExternalText>
        </Field>
      )}
      {dRep?.metadataHash && (
        <Field label={t("drepDashboard.overview.identity.metadataHash")}>
          <IdRow label={t("drepDashboard.overview.identity.metadataHash")} value={dRep.metadataHash} />
        </Field>
      )}
      <Divider />
      <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0 }}>{t("drepDashboard.overview.identity.links")}</Box>
      {links.length ? (
        links.map((ref) => (
          <Field key={`${ref.label}-${ref.uri}`} label={ref.label}>
            <ExternalText href={ref.uri}>{ref.uri}</ExternalText>
          </Field>
        ))
      ) : (
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t("drepDashboard.overview.identity.noLinks")}</Box>
      )}
    </Panel>
  );
};

const ReferencesCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const refs = data.dRep?.linkReferences ?? [];
  return (
    <Panel
      action={!refs.length ? <ArrowLink to={PATHS.editDrepMetadata}>{t("drepDashboard.overview.references.add")}</ArrowLink> : undefined}
      testId="drep-references-card"
      title={t("drepDashboard.overview.references.title")}
      titleId="drep-references-title"
    >
      {refs.length ? (
        refs.map((ref) => (
          <Field key={`${ref.label}-${ref.uri}`} label={ref.label}>
            <ExternalText href={ref.uri}>{ref.uri}</ExternalText>
          </Field>
        ))
      ) : (
        <Box component="p" sx={{ color: ds.gray["600"], fontSize: 14, lineHeight: "20px", m: 0 }}>{t("drepDashboard.overview.references.empty")}</Box>
      )}
    </Panel>
  );
};

const DetailsCard = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const source = { ...data.dRep, ...data.voter };
  const fields = ["objectives", "motivations", "qualifications"] as const;
  return (
    <Panel
      action={(
        <Box
          aria-label={t("drepDashboard.overview.details.edit")}
          component={RouterLink}
          data-testid="drep-details-edit"
          sx={{ borderRadius: "4px", color: ds.gray["600"], display: "flex", p: 0.5, "&:hover": { color: ds.gray["900"] }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
          to={PATHS.editDrepMetadata}
        >
          <PenLine aria-hidden size={18} />
        </Box>
      )}
      testId="drep-details-card"
      title={t("drepDashboard.overview.details.title")}
      titleId="drep-details-title"
    >
      {fields.map((field) => (
        <Box key={field}>
          <Box component="h3" sx={{ color: ds.gray["900"], fontSize: 16, fontWeight: 600, m: 0 }}>
            {t(`drepDashboard.overview.details.${field}`)}
          </Box>
          <Box component="p" sx={{ color: source[field] ? ds.gray["600"] : ds.gray["500"], fontSize: 14, lineHeight: "20px", m: 0, mt: 0.75, overflowWrap: "anywhere", whiteSpace: "pre-line" }}>
            {source[field] || t("drepDashboard.overview.details.notProvided")}
          </Box>
        </Box>
      ))}
    </Panel>
  );
};

/** A vote choice as a coloured dot and upper-case word. */
export const VoteMark = ({ vote }: { vote: "yes" | "no" | "abstain" }) => {
  const { t } = useTranslation();
  return (
    <Box component="span" sx={{ alignItems: "center", color: VOTE_TEXT[vote], display: "inline-flex", fontSize: 14, fontWeight: 600, gap: 0.75, textTransform: "uppercase" }}>
      <Box aria-hidden component="span" sx={{ backgroundColor: VOTE_COLORS[vote], borderRadius: "50%", height: 8, width: 8 }} />
      {t(`drepDashboard.vote.${vote}`)}
    </Box>
  );
};

export const sortedVotes = (data: DashboardData) =>
  [...(data.votes ?? [])].sort(
    (a, b) => new Date(b.vote?.date || 0).getTime() - new Date(a.vote?.date || 0).getTime(),
  );

const RecentVotes = ({ data }: { data: DashboardData }) => {
  const { t } = useTranslation();
  const rows = sortedVotes(data).filter((row) => row.vote).slice(0, 3);
  const total = data.votes?.filter((row) => row.vote).length ?? 0;
  const thisEpoch =
    data.votes?.filter((row) => row.vote?.epochNo === data.currentEpoch).length ?? 0;

  let body: ReactNode;
  if (data.votesLoading) body = <Skeleton height={180} variant="rounded" />;
  else if (data.votesError) {
    body = (
      <Box role="alert" sx={{ color: ds.gray["600"], fontSize: 14 }}>
        {t("drepDashboard.overview.recent.error")}{" "}
        <Box component="button" onClick={data.retryVotes} sx={{ background: "none", border: "none", color: ds.primary["600"], cursor: "pointer", fontSize: 14, fontWeight: 600, p: 0 }} type="button">
          {t("drepDashboard.retry")}
        </Box>
      </Box>
    );
  } else if (!rows.length) {
    body = (
      <EmptyState
        actions={(
          <Button component={RouterLink} endIcon={<ArrowRight aria-hidden size={16} />} size="medium" to={PATHS.dashboardGovernanceActions}>
            {t("drepDashboard.overview.recent.browse")}
          </Button>
        )}
        body={t("drepDashboard.overview.recent.emptyBody")}
        icon={<Archive size={20} />}
        testId="drep-recent-votes-empty"
        title={t("drepDashboard.overview.recent.emptyTitle")}
      />
    );
  } else {
    body = (
      <TableFrame label={t("drepDashboard.overview.recent.title")} minWidth={620}>
        <thead>
          <tr>
            {[t("drepDashboard.overview.recent.action"), t("drepDashboard.overview.recent.vote"), t("drepDashboard.overview.recent.castOn"), t("drepDashboard.overview.recent.outcome")].map((heading) => (
              <Box key={heading} component="th" scope="col" sx={{ ...th, textTransform: "uppercase" }}>{heading}</Box>
            ))}
          </tr>
        </thead>
        <tbody>
          {rows.map(({ proposal, vote }) => {
            const id = getFullGovActionId(proposal.txHash, proposal.index);
            const outcome = actionOutcome(
              data.actionsByKey.get(actionKey(proposal.txHash, proposal.index)),
            );
            return (
              <tr key={id} data-testid="drep-recent-vote-row">
                <Box component="td" sx={td}>
                  <Box component={RouterLink} sx={{ color: ds.gray["900"], display: "block", fontWeight: 600, textDecoration: "none", "&:hover": { textDecoration: "underline" } }} to={`${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${id}`}>
                    {proposal.title || t("govActions.card.untitled")}
                  </Box>
                  <Box sx={{ color: ds.gray["600"], fontSize: 13 }}>{getProposalTypeLabel(proposal.type)}</Box>
                </Box>
                <Box component="td" sx={td}>{vote && <VoteMark vote={vote.vote} />}</Box>
                <Box component="td" sx={{ ...td, color: ds.gray["600"], whiteSpace: "nowrap" }}>
                  {vote && t("drepDashboard.overview.recent.castOnValue", { date: formatShortDate(vote.date), epoch: vote.epochNo })}
                </Box>
                <Box component="td" sx={{ ...td, fontWeight: 500 }}>{outcome === "unknown" ? "" : t(`drepDashboard.outcome.${outcome}`)}</Box>
              </tr>
            );
          })}
        </tbody>
      </TableFrame>
    );
  }

  return (
    <Panel
      action={rows.length ? (
        <ArrowLink testId="drep-recent-votes-view-all" to={`${PATHS.dashboard}?tab=governance`}>
          {t("drepDashboard.overview.recent.viewAll")}
        </ArrowLink>
      ) : (
        <Box sx={{ color: ds.gray["600"], fontSize: 14 }}>
          {t("drepDashboard.overview.recent.totals", { epoch: thisEpoch, total })}
        </Box>
      )}
      subtitle={rows.length ? t("drepDashboard.overview.recent.subtitle") : undefined}
      testId="drep-recent-votes"
      title={t("drepDashboard.overview.recent.title")}
      titleId="drep-recent-votes-title"
    >
      {body}
    </Panel>
  );
};

/** The left column every overview-style tab shares. */
export const SideColumn = ({
  data,
  walletCard,
  full = true,
}: {
  data: DashboardData;
  walletCard: ReactNode;
  full?: boolean;
}) => (
  <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
    {walletCard}
    <RepresentationCard data={data} />
    {full && <IdentityCard data={data} />}
    {full && <ReferencesCard data={data} />}
  </Box>
);

export const twoColumns = {
  alignItems: "start",
  display: "grid",
  gap: 2.5,
  gridTemplateColumns: { xxs: "minmax(0, 1fr)", lg: "380px minmax(0, 1fr)" },
};

export const OverviewTab = ({
  data,
  walletCard,
  actionCards,
}: {
  data: DashboardData;
  walletCard: ReactNode;
  actionCards: ReactNode;
}) => (
  <Box sx={twoColumns}>
    <SideColumn data={data} walletCard={walletCard} />
    <Box sx={{ display: "flex", flexDirection: "column", gap: 2.5, minWidth: 0 }}>
      <DetailsCard data={data} />
      {actionCards}
      <RecentVotes data={data} />
    </Box>
  </Box>
);
