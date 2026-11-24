import { ReactNode } from "react";
import { useTranslation } from "react-i18next";
import { Box } from "@mui/material";
import { ExternalLink } from "lucide-react";

import { GovernanceActionAuthors } from "@/components/molecules/GovernanceActionAuthors";
import { CopyButton } from "@/components/organisms/DRepDashboard/parts";
import { ds, RADIUS } from "@/consts/designTokens";
import type { GovernanceActionRecord } from "@/models/governanceActions";
import { correctVoteAdaFormat } from "@/utils/adaFormat";
import { getFullGovActionId } from "@/utils/getGovActionId";
import { getGovernanceActionCIP129Id } from "@/utils/governanceActions";
import { openInNewTab } from "@/utils/openInNewTab";
import { lifecycleEvents, outcomeKey, treasuryWithdrawals } from "@/utils/outcomeDetails";

import { Card, CardTitle, dateWithEpoch, Divider } from "./parts";

const TIME_FORMAT = "EEE MMM dd, yyyy, hh:mm:ss a";

type FieldProps = { label: string; children: ReactNode; testId?: string };

const Field = ({ label, children, testId }: FieldProps) => (
  <Box data-testid={testId} sx={{ minWidth: 0 }}>
    <Box sx={{ color: ds.gray["600"], fontSize: 12 }}>{label}</Box>
    <Box sx={{ color: ds.gray["900"], fontSize: 13, mt: 0.5 }}>{children}</Box>
  </Box>
);

/** A long value on one line, cut with an ellipsis, with a copy or open button. */
const LongValue = ({
  value,
  copyLabel,
  isLink = false,
  testId,
}: {
  value: string;
  copyLabel: string;
  isLink?: boolean;
  testId?: string;
}) => (
  <Box sx={{ alignItems: "center", display: "flex", gap: 0.5, minWidth: 0 }}>
    <Box
      component="span"
      data-testid={testId}
      sx={{ color: ds.primary["600"], flex: 1, minWidth: 0, overflow: "hidden", textOverflow: "ellipsis", whiteSpace: "nowrap" }}
      title={value}
    >
      {value}
    </Box>
    {isLink ? (
      <Box
        aria-label={copyLabel}
        component="button"
        onClick={() => openInNewTab(value)}
        sx={{ background: "none", border: "none", borderRadius: `${RADIUS.xs}px`, color: ds.gray["500"], cursor: "pointer", display: "flex", p: 0.5, "&:hover": { color: ds.gray["800"] }, "&:focus-visible": { outline: `2px solid ${ds.primary["600"]}` } }}
        type="button"
      >
        <ExternalLink aria-hidden size={14} />
      </Box>
    ) : (
      <CopyButton label={copyLabel} value={value} />
    )}
  </Box>
);

/** Ids, metadata anchor, receiving address, authors and the action's dates. */
export const ActionInformationCard = ({
  action,
  authors,
}: {
  action: GovernanceActionRecord;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  authors?: any;
}) => {
  const { t } = useTranslation();
  const id105 = getFullGovActionId(action.tx_hash, action.index);
  const id129 = getGovernanceActionCIP129Id(action);
  const withdrawals = treasuryWithdrawals(action);
  const ended = lifecycleEvents(action).filter((event) => event.key !== "submitted");
  const outcome = outcomeKey(action);

  return (
    <Card testId="outcome-action-information" sx={{ display: "flex", flexDirection: "column", gap: 2 }}>
      <CardTitle>{t("outcome.info.title")}</CardTitle>
      <Field label={t("outcome.info.id105")} testId="single-action-CIP-105-id">
        <LongValue copyLabel={t("outcome.info.id105")} value={id105} />
      </Field>
      <Field label={t("outcome.info.id129")} testId="single-action-CIP-129-id">
        <LongValue copyLabel={t("outcome.info.id129")} value={id129} />
      </Field>
      <Divider />
      {action.url && (
        <Field label={t("outcome.info.anchorLink")} testId="metadata-anchor-link">
          <LongValue copyLabel={t("outcome.info.anchorLink")} isLink value={action.url} />
        </Field>
      )}
      {action.data_hash && (
        <Field label={t("outcome.info.anchorHash")} testId="metadata-anchor-hash">
          <LongValue copyLabel={t("outcome.info.anchorHash")} value={action.data_hash} />
        </Field>
      )}
      {withdrawals.map((withdrawal, index) => (
        // eslint-disable-next-line react/no-array-index-key
        <Box key={index} sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
          <Field label={t("outcome.info.receivingAddress")} testId={`outcome-withdrawal-${index}-address`}>
            <LongValue copyLabel={t("outcome.info.receivingAddress")} value={withdrawal.receivingAddress} />
          </Field>
          <Field label={t("outcome.info.amount")} testId={`outcome-withdrawal-${index}-amount`}>
            {`₳ ${correctVoteAdaFormat(Number(withdrawal.amount))}`}
          </Field>
        </Box>
      ))}
      <GovernanceActionAuthors authors={authors} metadataUrl={action.url} />
      <Field label={t("outcome.info.epoch")}>{action.epoch_no}</Field>
      <Field label={t("outcome.info.submitted")}>
        {dateWithEpoch(t, action.time, action.epoch_no, TIME_FORMAT)}
      </Field>
      {ended.map((event) => (
        <Field key={event.key} label={t(`outcome.statuses.${event.key}`)}>
          {dateWithEpoch(t, event.time, event.epoch, TIME_FORMAT)}
        </Field>
      ))}
      {outcome === "live" || action.expiry_date ? (
        <Field label={t("outcome.info.votingEndsOn")}>
          {dateWithEpoch(t, action.expiry_date, action.expiration, "MMM dd, yyyy")}
        </Field>
      ) : null}
    </Card>
  );
};
