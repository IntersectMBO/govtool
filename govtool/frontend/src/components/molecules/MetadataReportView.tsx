import { PropsWithChildren } from "react";
import {
  Box,
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableRow,
} from "@mui/material";

import { Typography } from "@atoms";
import { useTranslation } from "@hooks";
import type { ConnectAttempt, FetchHop, MetadataReport } from "@models";

import { MetadataReportBody } from "./MetadataReportBody";

const formatTimestamp = (value: string) => {
  const date = new Date(value);
  return Number.isNaN(date.getTime()) ? value : date.toLocaleString();
};

const breakAnywhere = { wordBreak: "break-all", overflowWrap: "anywhere" };

const Field = ({
  label,
  children,
  mono,
}: PropsWithChildren<{ label: string; mono?: boolean }>) => (
  <Box sx={{ display: "flex", flexWrap: "wrap", columnGap: 1 }}>
    <Typography variant="body2" color="neutralGray">
      {`${label}:`}
    </Typography>
    <Typography
      variant="body2"
      sx={{ ...breakAnywhere, ...(mono && { fontFamily: "monospace" }) }}
    >
      {children}
    </Typography>
  </Box>
);

const useOutcomeLabel = () => {
  const { t } = useTranslation();
  return (attempt: ConnectAttempt) => {
    if (attempt.outcome === "blocked" && attempt.blockedRange) {
      return t("metadataReport.outcomes.blockedRange", {
        range: attempt.blockedRange,
      });
    }
    if (attempt.outcome === "timeout" && attempt.timeoutStage) {
      return t("metadataReport.outcomes.timeoutStage", {
        stage: t(`metadataReport.timeoutStages.${attempt.timeoutStage}`),
      });
    }
    return t(`metadataReport.outcomes.${attempt.outcome}`, {
      defaultValue: attempt.outcome,
    });
  };
};

const AttemptsTable = ({ attempts }: { attempts: ConnectAttempt[] }) => {
  const { t } = useTranslation();
  const outcomeLabel = useOutcomeLabel();

  if (attempts.length === 0) {
    return (
      <Typography variant="body2">
        {t("metadataReport.hop.noAttempts")}
      </Typography>
    );
  }

  return (
    <Box sx={{ overflowX: "auto" }}>
      <Table size="small" data-testid="metadata-report-attempts">
        <TableHead>
          <TableRow>
            <TableCell>{t("metadataReport.attempt.address")}</TableCell>
            <TableCell>{t("metadataReport.attempt.family")}</TableCell>
            <TableCell>{t("metadataReport.attempt.outcome")}</TableCell>
            <TableCell>{t("metadataReport.attempt.errorCode")}</TableCell>
            <TableCell>{t("metadataReport.attempt.message")}</TableCell>
            <TableCell>{t("metadataReport.attempt.timings")}</TableCell>
          </TableRow>
        </TableHead>
        <TableBody>
          {attempts.map((attempt, index) => {
            const { connectMs, tlsMs, firstByteMs, endMs } = attempt.timings;
            const timings = [
              connectMs !== undefined &&
                t("metadataReport.timings.connect", { ms: connectMs }),
              tlsMs !== undefined &&
                t("metadataReport.timings.tls", { ms: tlsMs }),
              firstByteMs !== undefined &&
                t("metadataReport.timings.firstByte", { ms: firstByteMs }),
              endMs !== undefined &&
                t("metadataReport.timings.end", { ms: endMs }),
            ]
              .filter(Boolean)
              .join(", ");
            return (
              // eslint-disable-next-line react/no-array-index-key
              <TableRow key={`${attempt.address}-${index}`}>
                <TableCell sx={{ fontFamily: "monospace", ...breakAnywhere }}>
                  {attempt.address}
                </TableCell>
                <TableCell>
                  {t("metadataReport.attempt.familyValue", {
                    family: attempt.family,
                  })}
                </TableCell>
                <TableCell
                  sx={{
                    color:
                      attempt.outcome === "connected" ? undefined : "errorRed",
                  }}
                >
                  {outcomeLabel(attempt)}
                </TableCell>
                <TableCell sx={{ fontFamily: "monospace" }}>
                  {attempt.errorCode ?? "—"}
                </TableCell>
                <TableCell sx={breakAnywhere}>
                  {attempt.message ?? "—"}
                </TableCell>
                <TableCell>{timings || "—"}</TableCell>
              </TableRow>
            );
          })}
        </TableBody>
      </Table>
    </Box>
  );
};

const HopView = ({ hop, number }: { hop: FetchHop; number: number }) => {
  const { t } = useTranslation();
  const headers = hop.response ? Object.entries(hop.response.headers) : [];

  return (
    <Box
      data-testid={`metadata-report-hop-${number}`}
      sx={{
        display: "flex",
        flexDirection: "column",
        gap: 1,
        p: 2,
        border: "1px solid #E6E6F0",
        borderRadius: 1,
      }}
    >
      <Typography variant="body2" fontWeight={600}>
        {t("metadataReport.hop.title", { number })}
      </Typography>
      <Field label={t("metadataReport.summary.url")} mono>
        {hop.url}
      </Field>

      {"error" in hop.dns ? (
        <Field label={t("metadataReport.hop.dnsError")} mono>
          {`${hop.dns.error.code}: ${hop.dns.error.message}`}
        </Field>
      ) : (
        <Field label={t("metadataReport.hop.dnsAddresses")} mono>
          {hop.dns.addresses.length > 0
            ? hop.dns.addresses.map(({ address }) => address).join(", ")
            : t("metadataReport.hop.dnsNoAddresses")}
        </Field>
      )}

      <Typography variant="body2" color="neutralGray">
        {t("metadataReport.hop.attempts")}
      </Typography>
      <AttemptsTable attempts={hop.attempts} />

      {hop.response ? (
        <>
          <Field label={t("metadataReport.hop.httpStatus")}>
            {hop.response.status}
          </Field>
          {headers.length > 0 && (
            <Box>
              <Typography variant="body2" color="neutralGray">
                {t("metadataReport.hop.headers")}
              </Typography>
              <Box
                component="dl"
                sx={{
                  m: 0,
                  display: "grid",
                  gridTemplateColumns: "max-content 1fr",
                  columnGap: 2,
                  fontFamily: "monospace",
                  fontSize: 12,
                  "& dd": { m: 0, ...breakAnywhere },
                }}
              >
                {headers.map(([name, value]) => (
                  <Box key={name} sx={{ display: "contents" }}>
                    <dt>{name}</dt>
                    <dd>{Array.isArray(value) ? value.join(", ") : value}</dd>
                  </Box>
                ))}
              </Box>
            </Box>
          )}
        </>
      ) : (
        <Typography variant="body2">
          {t("metadataReport.hop.noResponse")}
        </Typography>
      )}

      {hop.redirectTo && (
        <Field label={t("metadataReport.hop.redirectTo")} mono>
          {hop.redirectTo}
        </Field>
      )}
    </Box>
  );
};

/**
 * Everything one fetch report holds (spec §2.7). Nothing is hidden (D116);
 * everything from the remote side is shown as plain text.
 */
export const MetadataReportView = ({ report }: { report: MetadataReport }) => {
  const { t } = useTranslation();
  const { result } = report;

  return (
    <Box
      data-testid="metadata-report"
      sx={{ display: "flex", flexDirection: "column", gap: 2 }}
    >
      <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}>
        <Typography
          variant="body2"
          fontWeight={600}
          data-testid="metadata-report-category"
        >
          {t(`metadataReport.categories.${result.category}`, {
            defaultValue: result.category,
          })}
        </Typography>
        <Field label={t("metadataReport.summary.code")} mono>
          {result.code}
        </Field>
        <Field label={t("metadataReport.summary.message")}>
          {result.message}
        </Field>
        <Field label={t("metadataReport.summary.startedAt")}>
          {formatTimestamp(report.startedAt)}
        </Field>
        <Field label={t("metadataReport.summary.finishedAt")}>
          {formatTimestamp(report.finishedAt)}
        </Field>
        <Field label={t("metadataReport.summary.url")} mono>
          {report.url}
        </Field>
        {report.effectiveUrl !== report.url && (
          <Field label={t("metadataReport.summary.effectiveUrl")} mono>
            {report.effectiveUrl}
          </Field>
        )}
      </Box>

      {result.code === "HASH_MISMATCH" && (
        <Box
          data-testid="metadata-report-hash-mismatch"
          sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}
        >
          <Typography variant="body2" sx={{ color: "errorRed" }}>
            {t("metadataReport.summary.hashMismatch")}
          </Typography>
          <Field label={t("metadataReport.summary.requestedHash")} mono>
            {report.hash}
          </Field>
          <Field label={t("metadataReport.summary.servedHash")} mono>
            {result.servedHash ?? "—"}
          </Field>
        </Box>
      )}

      {report.hops.map((hop, index) => (
        // eslint-disable-next-line react/no-array-index-key
        <HopView key={index} hop={hop} number={index + 1} />
      ))}

      {/* A mismatch references the served document by its hash rather than
          storing a copy (D114), so a missing body there is expected. */}
      {(report.body || result.code !== "HASH_MISMATCH") && (
        <MetadataReportBody body={report.body} issues={result.issues ?? []} />
      )}
    </Box>
  );
};
