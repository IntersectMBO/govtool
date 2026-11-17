import type { TFunction } from "i18next";
import {
  Clock,
  Info,
  Landmark,
  Newspaper,
  Send,
  Sparkles,
  UserRoundX,
  type LucideIcon,
} from "lucide-react";

import { GOV_ACTION_HISTORY_PATHS, PATHS } from "@/consts/paths";
import { ds } from "@/consts/designTokens";
import type { GovToolNotification } from "@/models";
import { correctAdaFormatWithSuffix } from "@/utils/adaFormat";
import { getProposalTypeLabel } from "@/utils/getProposalTypeLabel";
import { getGovernanceActionCIP129Id } from "@/utils/governanceActions";
import { daysLeft, formatUtc } from "@/utils/governanceActivity";
import { shortId } from "@/utils/globalSearch";

export type Tone = "primary" | "success" | "warning" | "error" | "neutral";

export const TONES: Record<Tone, { bg: string; fg: string; text: string }> = {
  primary: { bg: ds.primary["100"], fg: ds.primary["600"], text: ds.primary["700"] },
  success: { bg: ds.success["100"], fg: ds.success["600"], text: ds.success["700"] },
  warning: { bg: ds.warning["100"], fg: ds.warning["600"], text: ds.warning["700"] },
  error: { bg: ds.error["100"], fg: ds.error["600"], text: ds.error["700"] },
  neutral: { bg: ds.gray["100"], fg: ds.gray["600"], text: ds.gray["700"] },
};

export type NotificationAction = { label: string; path?: string; href?: string };

export type NotificationContent = {
  category: string;
  short: string;
  icon: LucideIcon;
  tone: Tone;
  status?: { label: string; tone: Tone };
  title: string;
  body: string;
  reason?: string;
  context?: string;
  actions: NotificationAction[];
};

/** Links from the service open only over http(s). */
export const safeHref = (url: string | null | undefined) =>
  (url && /^https?:\/\//i.test(url) ? url : undefined);

const dRepPath = (id: string) => PATHS.dashboardDRepDirectoryDRep.replace(":dRepId", id);

const historyPath = (actionId: string) => {
  const [txHash, index] = actionId.split("#");
  return `${GOV_ACTION_HISTORY_PATHS.governanceActionHistory}/${getGovernanceActionCIP129Id({
    tx_hash: txHash,
    index: Number(index ?? 0),
  })}`;
};

const actionContext = (actionType: string, actionId: string) => {
  const [txHash, index] = actionId.split("#");
  const cip129 = getGovernanceActionCIP129Id({ tx_hash: txHash, index: Number(index ?? 0) });
  return `${getProposalTypeLabel(actionType)} · ${shortId(cip129)}`;
};

/** The words, icon and links of one notification; the same for both modes. */
export const describeNotification = (
  item: GovToolNotification,
  t: TFunction,
  now = new Date(),
): NotificationContent => {
  const k = (key: string, options?: Record<string, unknown>) =>
    t(`notifications.kinds.${item.kind}.${key}`, options);
  const power = (lovelace: number | string | null) =>
    (lovelace === null || !Number.isFinite(Number(lovelace))
      ? t("notifications.yourVotingPower")
      : t("notifications.votingPower", { amount: correctAdaFormatWithSuffix(Number(lovelace)) }));

  switch (item.kind) {
    case "drepChange": {
      const { data } = item;
      const name = data.dRepName || shortId(data.dRepId);
      const params = { name, power: power(data.votingPower), epoch: data.epoch };
      const isUrgent = data.change !== "metadataUpdated";
      return {
        category: k("category"),
        short: k("short"),
        icon: UserRoundX,
        tone: isUrgent ? "error" : "warning",
        status: isUrgent ? { label: k("actionNeeded"), tone: "error" } : undefined,
        title: k(`${data.change === "metadataUpdated" ? "updated" : data.change}Title`, params),
        body: k(`${data.change === "metadataUpdated" ? "updated" : data.change}Body`, params),
        reason: k("reason"),
        actions: [
          ...(isUrgent ? [{ label: k("choose"), path: PATHS.dashboardDRepDirectory }] : []),
          { label: k("viewProfile", { name }), path: dRepPath(data.dRepId) },
        ],
      };
    }
    case "delegationChange": {
      const { data } = item;
      const removed = data.change === "removed";
      const name = data.target.dRepName || (data.target.dRepId ? shortId(data.target.dRepId) : "");
      let title = k("removedTitle");
      if (!removed) {
        if (data.target.type === "abstain") title = k("abstainTitle");
        else if (data.target.type === "noConfidence") title = k("noConfidenceTitle");
        else title = k("drepTitle", { name });
      }
      return {
        category: k("category"),
        short: k("short"),
        icon: Send,
        tone: removed ? "neutral" : "success",
        status: removed
          ? { label: k("removed"), tone: "neutral" }
          : { label: k("confirmed"), tone: "success" },
        title,
        body: removed ? k("removedBody") : k("body", { power: power(data.votingPower) }),
        actions:
          !removed && data.target.type === "drep" && data.target.dRepId
            ? [{ label: k("viewProfile"), path: dRepPath(data.target.dRepId) }]
            : [{ label: k("viewDirectory"), path: PATHS.dashboardDRepDirectory }],
      };
    }
    case "actionActivity": {
      const { data } = item;
      const title = data.title || getProposalTypeLabel(data.actionType);
      const vote = data.vote ? k(`votes.${data.vote}`) : k("noVote");
      return {
        category: data.relation === "voted" ? k("category") : k("dRepCategory"),
        short: k("short"),
        icon: data.outcome === "expired" ? Clock : Landmark,
        tone: data.outcome === "expired" ? "neutral" : "success",
        status: {
          label: k(data.outcome),
          tone: data.outcome === "expired" ? "neutral" : "success",
        },
        title: k(`${data.outcome}Title`, { title }),
        body: data.relation === "voted" ? k("votedBody", { vote }) : k("dRepVotedBody", { vote }),
        context: actionContext(data.actionType, data.actionId),
        actions: [{ label: k("view"), path: historyPath(data.actionId) }],
      };
    }
    case "relevantAction": {
      const { data } = item;
      const left = daysLeft(data.expiryDate ?? undefined, now);
      const title = data.title || getProposalTypeLabel(data.actionType);
      return {
        category: k("category"),
        short: k("short"),
        icon: Sparkles,
        tone: "primary",
        status: left
          ? {
            label: left.days > 0 ? k("daysLeft", { count: left.days }) : k("lessThanDay"),
            tone: left.tone === "neutral" ? "primary" : left.tone,
          }
          : undefined,
        title: k("title", { title }),
        body:
          data.reason === "deadline"
            ? k("body", { when: formatUtc(data.expiryDate, "dateTime") })
            : k("matchedBody"),
        context: actionContext(data.actionType, data.actionId),
        actions: [
          { label: k("review"), path: `${PATHS.dashboardGovernanceActions}/${data.actionId}` },
        ],
      };
    }
    case "digest": {
      const { data } = item;
      return {
        category: k("category"),
        short: k("short"),
        icon: Newspaper,
        tone: "neutral",
        title: k("title", { count: data.total }),
        body: k("body", { from: formatUtc(data.from, "date"), to: formatUtc(data.to, "date") }),
        actions: [
          safeHref(data.url)
            ? { label: k("open"), href: safeHref(data.url) }
            : { label: k("open"), path: PATHS.dashboardGovernanceActions },
        ],
      };
    }
    case "system":
    default: {
      const data = item.data as { title: string; body: string; url: string | null };
      return {
        category: t("notifications.kinds.system.category"),
        short: t("notifications.kinds.system.short"),
        icon: Info,
        tone: "primary",
        status: { label: t("notifications.kinds.system.badge"), tone: "primary" },
        title: data.title,
        body: data.body,
        actions: safeHref(data.url)
          ? [{ label: t("notifications.kinds.system.learnMore"), href: safeHref(data.url) }]
          : [],
      };
    }
  }
};
