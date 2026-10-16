import { act, fireEvent, render, screen } from "@testing-library/react";
import { beforeEach, describe, expect, it, vi } from "vitest";

import "@/i18n";
import type {
  MetadataFailure,
  MetadataRefreshOutcome,
  MetadataReport,
  MetadataResult,
} from "@models";

import { MetadataFailureDetails } from "./MetadataFailureDetails";

const state: {
  result: MetadataResult | undefined;
  reports: { id: string; startedAt: string; code: string }[];
  reportsById: Record<string, MetadataReport>;
} = { result: undefined, reports: [], reportsById: {} };

const reportQuery = vi.fn();
const retry = vi.fn<() => Promise<MetadataRefreshOutcome>>();
const addSuccessAlert = vi.fn();

vi.mock("@atoms", async () => ({
  Button: (await import("../atoms/Button")).Button,
  Typography: (await import("../atoms/Typography")).Typography,
}));

vi.mock("@context", () => ({
  useSnackbar: () => ({ addSuccessAlert }),
}));

vi.mock("@/hooks/mutations", () => ({
  useMetadataRetryMutation: () => ({ retry, isRetrying: false }),
}));

vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
  useCountdown: (await import("@/hooks/useCountdown")).useCountdown,
  useGetMetadataResolveQuery: () => ({ metadataResult: state.result }),
  useGetMetadataReportsQuery: () => ({ reports: state.reports }),
  useGetMetadataReportQuery: (id: string | null, enabled: boolean) => {
    reportQuery(id, enabled);
    return {
      report: enabled && id ? state.reportsById[id] : undefined,
      isLoading: false,
      isError: false,
    };
  },
}));

const anchor = { url: "https://example.com/doc.json", hash: "ab".repeat(32) };

const failure = (reportId: string): MetadataFailure => ({
  ok: false,
  code: "FETCH_ERROR",
  category: "NETWORK",
  message: "connect ECONNREFUSED",
  reportId,
  checkedAt: "2026-09-24T10:00:00Z",
});

const report = (id: string, message: string): MetadataReport => ({
  id,
  hash: anchor.hash,
  url: anchor.url,
  effectiveUrl: anchor.url,
  startedAt: "2026-09-24T10:00:00Z",
  finishedAt: "2026-09-24T10:00:01Z",
  hops: [],
  result: { code: "FETCH_ERROR", category: "NETWORK", message, issues: [] },
});

describe("MetadataFailureDetails", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    state.result = failure("r1");
    state.reports = [];
    state.reportsById = { r1: report("r1", "first attempt failed") };
  });

  it("renders nothing when the backend has no failure for the anchor", () => {
    state.result = undefined;
    const { container } = render(<MetadataFailureDetails anchor={anchor} />);
    expect(container).toBeEmptyDOMElement();
  });

  it("is closed by default and loads the report only when opened", () => {
    render(<MetadataFailureDetails anchor={anchor} />);

    const button = screen.getByTestId("metadata-error-report-button");
    expect(button).toHaveTextContent("View Error Report");
    expect(screen.queryByTestId("metadata-report")).not.toBeInTheDocument();
    expect(reportQuery).toHaveBeenLastCalledWith("r1", false);

    fireEvent.click(button);

    expect(reportQuery).toHaveBeenLastCalledWith("r1", true);
    expect(screen.getByTestId("metadata-report-dialog")).toBeInTheDocument();
    expect(screen.getByTestId("metadata-report")).toBeInTheDocument();
    expect(screen.getByText("first attempt failed")).toBeInTheDocument();
    expect(
      screen.getByTestId("metadata-report-documentation"),
    ).toHaveTextContent("Read documentation");
  });

  it("offers only the latest ten reports, however many the backend lists", () => {
    state.reports = Array.from({ length: 12 }, (_, i) => ({
      id: `r${i + 1}`,
      startedAt: `2026-09-24T10:${String(59 - i).padStart(2, "0")}:00Z`,
      code: "FETCH_ERROR",
    }));
    render(<MetadataFailureDetails anchor={anchor} />);
    fireEvent.click(screen.getByTestId("metadata-error-report-button"));

    fireEvent.mouseDown(screen.getByRole("combobox"));

    const options = screen.getAllByRole("option");
    expect(options).toHaveLength(10);
    expect(options[0]).toHaveTextContent("(latest)");
  });

  it("shows the report of a failed submission check even when the cache has the document", () => {
    state.result = { ok: true, hash: anchor.hash, body: {}, fetchedAt: "" };
    state.reportsById = { s1: report("s1", "Unexpected Status code: 404") };
    render(<MetadataFailureDetails anchor={anchor} reportId="s1" />);

    fireEvent.click(screen.getByTestId("metadata-error-report-button"));

    expect(reportQuery).toHaveBeenLastCalledWith("s1", true);
    expect(screen.getByText("Unexpected Status code: 404")).toBeInTheDocument();
  });

  it("shows why a check could not run when it has no report", () => {
    state.result = undefined;
    render(
      <MetadataFailureDetails
        anchor={anchor}
        error="timeout of 30000ms exceeded"
      />,
    );
    expect(screen.getByTestId("metadata-check-error")).toHaveTextContent(
      "timeout of 30000ms exceeded",
    );
    expect(
      screen.queryByTestId("metadata-error-report-button"),
    ).not.toBeInTheDocument();
  });

  it("shows no retry button unless allowed", () => {
    render(<MetadataFailureDetails anchor={anchor} />);
    fireEvent.click(screen.getByTestId("metadata-error-report-button"));
    expect(
      screen.queryByTestId("metadata-retry-button"),
    ).not.toBeInTheDocument();
  });

  it("reports success and lets the page reload after a retry resolves", async () => {
    const onRecovered = vi.fn();
    retry.mockImplementation(async () => {
      const ok: MetadataResult = {
        ok: true,
        hash: anchor.hash,
        body: {},
        fetchedAt: "2026-09-24T10:02:00Z",
      };
      state.result = ok;
      return { refetched: true, result: ok };
    });
    render(
      <MetadataFailureDetails
        anchor={anchor}
        canRetry
        onRecovered={onRecovered}
      />,
    );
    fireEvent.click(screen.getByTestId("metadata-error-report-button"));

    await act(async () => {
      fireEvent.click(screen.getByTestId("metadata-retry-button"));
    });

    expect(onRecovered).toHaveBeenCalledTimes(1);
    expect(addSuccessAlert).toHaveBeenCalledWith(
      "The metadata now loads correctly.",
    );
    expect(screen.getByTestId("metadata-retry-success")).toBeInTheDocument();
  });

  it("follows the new report after a retry that still fails", async () => {
    state.reportsById.r2 = report("r2", "second attempt failed");
    retry.mockImplementation(async () => {
      state.result = failure("r2");
      return { refetched: true, result: failure("r2") };
    });
    render(<MetadataFailureDetails anchor={anchor} canRetry />);
    fireEvent.click(screen.getByTestId("metadata-error-report-button"));
    expect(screen.getByText("first attempt failed")).toBeInTheDocument();

    await act(async () => {
      fireEvent.click(screen.getByTestId("metadata-retry-button"));
    });

    expect(screen.getByText("second attempt failed")).toBeInTheDocument();
    expect(screen.getByTestId("metadata-retry-notice")).toHaveTextContent(
      "still fails",
    );
  });
});
