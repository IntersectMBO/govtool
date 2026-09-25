import { useQuery } from "@tanstack/react-query";

import { QUERY_KEYS } from "@consts";
import {
  getMetadataReport,
  getMetadataReports,
  getMetadataResolve,
} from "@services";
import type { MetadataAnchor } from "@models";

/**
 * The backend's view of an anchor, carrying the failure's `reportId`. The
 * legacy validator still drives the existing labels; this only adds the report.
 * A backend without a metadata service answers 503, which surfaces as
 * `isError` and hides the diagnostics rather than breaking the page.
 */
export const useGetMetadataResolveQuery = (
  anchor: MetadataAnchor | null | undefined,
  enabled = true,
) => {
  const { data, isLoading, isError } = useQuery({
    queryKey: [QUERY_KEYS.useGetMetadataResolveKey, anchor?.hash, anchor?.url],
    queryFn: () =>
      getMetadataResolve(
        (anchor as MetadataAnchor).hash,
        (anchor as MetadataAnchor).url,
      ),
    enabled: enabled && !!anchor?.url && !!anchor?.hash,
    retry: false,
  });

  return { metadataResult: data, isLoading, isError };
};

/** Reports are immutable once written (D123), so one is never refetched. */
export const useGetMetadataReportQuery = (
  reportId: string | null | undefined,
  enabled = true,
) => {
  const { data, isLoading, isError } = useQuery({
    queryKey: [QUERY_KEYS.useGetMetadataReportKey, reportId],
    queryFn: () => getMetadataReport(reportId as string),
    enabled: enabled && !!reportId,
    staleTime: Infinity,
    retry: false,
  });

  return { report: data, isLoading, isError };
};

export const useGetMetadataReportsQuery = (
  anchor: MetadataAnchor | null | undefined,
  enabled = true,
) => {
  const { data, isLoading, isError } = useQuery({
    queryKey: [QUERY_KEYS.useGetMetadataReportsKey, anchor?.hash, anchor?.url],
    queryFn: () =>
      getMetadataReports(
        (anchor as MetadataAnchor).hash,
        (anchor as MetadataAnchor).url,
      ),
    enabled: enabled && !!anchor?.url && !!anchor?.hash,
    retry: false,
  });

  return { reports: data ?? [], isLoading, isError };
};
