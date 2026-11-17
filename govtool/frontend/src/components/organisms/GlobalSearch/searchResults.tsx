import { ReactNode } from "react";
import type { TFunction } from "i18next";

import { GOV_ACTION_HISTORY_PATHS, PATHS, PDF_PATHS } from "@/consts/paths";
import { useGetGovernanceActionMetadata } from "@/hooks/queries/useGetGovernanceActionMetadata";
import type { SearchCategory, useGlobalSearch } from "@/hooks/queries/useGlobalSearch";
import type {
  DRepData,
  ForumProposal,
  GovernanceActionRecord,
  ProposalData,
  StakeAddressLookup,
} from "@/models";
import type { HandleObject } from "@/models/adaHandle";
import { AutomatedVotingOptionCurrentDelegation } from "@/types/automatedVotingOptions";
import { correctAdaFormatWithSuffix } from "@/utils/adaFormat";
import { getFullGovActionId } from "@/utils/getGovActionId";
import { getProposalTypeLabel } from "@/utils/getProposalTypeLabel";
import { getGovernanceActionCIP129Id } from "@/utils/governanceActions";
import { decidedStatus } from "@/utils/governanceActionFormat";
import { formatUtc } from "@/utils/governanceActivity";
import { shortId, stakeAddressOf } from "@/utils/globalSearch";

import type { ResultBadge } from "./searchParts";

export type ResultTarget =
  | { kind: "navigate"; path: string; state?: unknown }
  | { kind: "copy"; value: string }
  | { kind: "search"; query: string };

export type SearchResult = {
  key: string;
  category: SearchCategory;
  title: ReactNode;
  description?: ReactNode;
  meta?: ReactNode;
  badge?: ResultBadge;
  target: ResultTarget;
};

type Context = { t: TFunction; isConnected: boolean };

const dRepPath = (view: string, isConnected: boolean) =>
  (isConnected ? PATHS.dashboardDRepDirectoryDRep : PATHS.dRepDirectoryDRep).replace(
    ":dRepId",
    view,
  );

const STATUS_TONES: Record<string, ResultBadge["tone"]> = {
  Active: "primary",
  Inactive: "neutral",
  Retired: "error",
};

/** An outcome's title, from its metadata when the list row has none. */
const OutcomeTitle = ({ record }: { record: GovernanceActionRecord }) => {
  const { metadata } = useGetGovernanceActionMetadata(record);
  return <>{record.title ?? metadata?.data?.title ?? getProposalTypeLabel(record.type)}</>;
};

const action = (proposal: ProposalData, { t, isConnected }: Context): SearchResult => {
  const id = getFullGovActionId(proposal.txHash, proposal.index);
  const cip129 = getGovernanceActionCIP129Id({ tx_hash: proposal.txHash, index: proposal.index });
  return {
    key: `actions-${id}`,
    category: "actions",
    title: proposal.title || getProposalTypeLabel(proposal.type),
    description: proposal.abstract,
    meta: `${shortId(cip129)} · ${t("globalSearch.action.submitted", {
      date: formatUtc(proposal.createdDate, "date"),
    })}`,
    badge: { label: t("globalSearch.action.badge"), tone: "primary" },
    target: {
      kind: "navigate",
      path: `${isConnected ? PATHS.dashboardGovernanceActions : PATHS.governanceActions}/${id}`,
      state: { proposal },
    },
  };
};

const dRep = (item: DRepData, { t, isConnected }: Context): SearchResult => ({
  key: `dreps-${item.view}`,
  category: "dreps",
  title: item.givenName || shortId(item.view),
  description: item.objectives ?? item.motivations ?? undefined,
  meta: [
    shortId(item.view),
    t("globalSearch.votingPower", { amount: correctAdaFormatWithSuffix(item.votingPower) }),
  ].join(" · "),
  badge: { label: item.status, tone: STATUS_TONES[item.status] ?? "neutral" },
  target: { kind: "navigate", path: dRepPath(item.view, isConnected) },
});

const proposal = (item: ForumProposal, { t }: Context): SearchResult => {
  const content = item.attributes.content?.attributes;
  const type = content?.gov_action_type?.attributes?.gov_action_type_name;
  const date = content?.createdAt ?? item.attributes.createdAt;
  return {
    key: `proposals-${item.id}`,
    category: "proposals",
    title: content?.prop_name ?? `#${item.id}`,
    description: content?.prop_abstract ?? undefined,
    meta: [type, date && formatUtc(date, "date")].filter(Boolean).join(" · "),
    badge: content?.prop_submitted
      ? { label: t("globalSearch.proposal.submitted"), tone: "success" }
      : { label: t("globalSearch.proposal.discussion"), tone: "warning" },
    target: { kind: "navigate", path: `${PDF_PATHS.proposalDiscussion}/${item.id}` },
  };
};

const outcome = (record: GovernanceActionRecord, { t }: Context): SearchResult => {
  const decided = decidedStatus(record);
  const epoch =
    record.status.enacted_epoch ??
    record.status.ratified_epoch ??
    record.status.expired_epoch ??
    record.status.dropped_epoch;
  const statusLabels = {
    enacted: t("activity.feed.kinds.enacted"),
    ratified: t("activity.feed.kinds.ratified"),
    expired: t("activity.feed.kinds.expired"),
  };
  return {
    key: `outcomes-${record.id}`,
    category: "outcomes",
    title: <OutcomeTitle record={record} />,
    description: record.abstract,
    meta: decided?.time
      ? t("globalSearch.outcome.meta", { date: formatUtc(decided.time, "date"), epoch })
      : getProposalTypeLabel(record.type),
    badge: decided && {
      label: statusLabels[decided.key],
      tone: decided.ratified ? "success" : "error",
    },
    target: {
      kind: "navigate",
      path: `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${record.id}`,
    },
  };
};

const delegationLine = (lookup: StakeAddressLookup, t: TFunction) => {
  const view = lookup.delegation?.dRepView;
  if (lookup.delegation === undefined) return undefined;
  if (!view) return t("globalSearch.address.notDelegated");
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_abstain)
    return t("globalSearch.address.abstain");
  if (view === AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence)
    return t("globalSearch.address.noConfidence");
  return t("globalSearch.address.delegatesTo", { target: shortId(view) });
};

const address = (lookup: StakeAddressLookup, { t, isConnected }: Context): SearchResult => {
  const view = lookup.delegation?.dRepView;
  const isDRep =
    !!view &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_abstain &&
    view !== AutomatedVotingOptionCurrentDelegation.drep_always_no_confidence;
  return {
    key: `addresses-${lookup.stakeAddress}`,
    category: "addresses",
    title: t("globalSearch.address.title"),
    description: shortId(lookup.stakeAddress),
    meta: [
      delegationLine(lookup, t),
      lookup.votingPower !== undefined &&
        t("globalSearch.votingPower", { amount: correctAdaFormatWithSuffix(lookup.votingPower) }),
      !isDRep && t("globalSearch.address.copyHint"),
    ]
      .filter(Boolean)
      .join(" · "),
    badge:
      lookup.isRegistered === undefined
        ? undefined
        : lookup.isRegistered
          ? { label: t("globalSearch.address.registered"), tone: "primary" }
          : { label: t("globalSearch.address.notRegistered"), tone: "neutral" },
    target: isDRep
      ? { kind: "navigate", path: dRepPath(view as string, isConnected) }
      : { kind: "copy", value: lookup.stakeAddress },
  };
};

const handle = (item: HandleObject, { t, isConnected }: Context): SearchResult => {
  const holderStake =
    stakeAddressOf(item.holder ?? "") ?? stakeAddressOf(item.resolved_addresses?.ada ?? "");
  const drep = item.drep?.cip_129;
  let target: ResultTarget = { kind: "copy", value: `$${item.name}` };
  if (drep) target = { kind: "navigate", path: dRepPath(drep, isConnected) };
  else if (holderStake) target = { kind: "search", query: holderStake };
  return {
    key: `handles-${item.name}`,
    category: "handles",
    title: `$${item.name}`,
    description: holderStake && t("globalSearch.handle.holder", { address: shortId(holderStake) }),
    meta: drep ? t("globalSearch.handle.opensDRep") : holderStake && t("globalSearch.handle.looksUp"),
    badge: drep
      ? { label: t("globalSearch.handle.drep"), tone: "primary" }
      : { label: t("globalSearch.handle.wallet"), tone: "neutral" },
    target,
  };
};

/** Every category's rows, in the order the dialog shows them. */
export const toResults = (
  search: ReturnType<typeof useGlobalSearch>,
  context: Context,
): Record<SearchCategory, SearchResult[]> => ({
  actions: search.actions.items.map((item) => action(item, context)),
  dreps: search.dreps.items.map((item) => dRep(item, context)),
  proposals: search.proposals.items.map((item) => proposal(item, context)),
  outcomes: search.outcomes.items.map((item) => outcome(item, context)),
  addresses: search.addresses.items.map((item) => address(item, context)),
  handles: search.handles.items.map((item) => handle(item, context)),
});

export const CATEGORY_ORDER: SearchCategory[] = [
  "actions",
  "dreps",
  "proposals",
  "outcomes",
  "addresses",
  "handles",
];
