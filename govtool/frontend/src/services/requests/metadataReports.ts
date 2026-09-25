import type {
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataReportSummary,
  MetadataResult,
} from "@models";

import { API } from "../API";

// Every call here resolves on any status and decides for itself. The shared
// API instance navigates to the error page on a 500, and failure diagnostics
// are advisory: a metadata service that is down (503) or misbehaving must
// leave the user on the page they are reading.
const acceptAnyStatus = { validateStatus: () => true } as const;

const fail = (path: string, status: number): never => {
  throw new Error(`${path} responded ${status}`);
};

export const getMetadataResolve = async (
  hash: string,
  url: string,
): Promise<MetadataResult> => {
  const response = await API.get<MetadataResult>("/metadata/resolve", {
    ...acceptAnyStatus,
    params: { hash, url },
  });
  if (response.status !== 200) fail("/metadata/resolve", response.status);
  return response.data;
};

export const postMetadataRetry = async (
  hash: string,
  url: string,
): Promise<MetadataRefreshOutcome> => {
  const response = await API.post<MetadataRefreshOutcome>(
    "/metadata/retry",
    { hash, url },
    acceptAnyStatus,
  );
  if (response.status !== 200) fail("/metadata/retry", response.status);
  return response.data;
};

/** One fetch report, or `null` when the backend has none under that id. */
export const getMetadataReport = async (
  reportId: string,
): Promise<MetadataReport | null> => {
  const response = await API.get<MetadataReport>(
    `/metadata/reports/${encodeURIComponent(reportId)}`,
    acceptAnyStatus,
  );
  if (response.status === 404) return null;
  if (response.status !== 200) fail("/metadata/reports/:id", response.status);
  return response.data;
};

/** Every report for (url, hash), newest first. */
export const getMetadataReports = async (
  hash: string,
  url: string,
): Promise<MetadataReportSummary[]> => {
  const response = await API.get<MetadataReportSummary[]>(
    "/metadata/reports",
    { ...acceptAnyStatus, params: { hash, url } },
  );
  if (response.status !== 200) fail("/metadata/reports", response.status);
  return Array.isArray(response.data) ? response.data : [];
};
