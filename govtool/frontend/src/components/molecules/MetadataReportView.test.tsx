import { fireEvent, render, screen, within } from "@testing-library/react";
import { beforeAll, describe, expect, it, vi } from "vitest";

import "@/i18n";
import type { MetadataReport } from "@models";

import { MetadataReportView } from "./MetadataReportView";

vi.mock("@atoms", async () => ({
  Button: (await import("../atoms/Button")).Button,
  Typography: (await import("../atoms/Typography")).Typography,
}));

vi.mock("@hooks", async () => ({
  useTranslation: (await import("react-i18next")).useTranslation,
}));

const scrollIntoView = vi.fn();
beforeAll(() => {
  Element.prototype.scrollIntoView = scrollIntoView;
});

const position = (offset: number, line: number, column: number) => ({
  offset,
  byteOffset: offset,
  line,
  column,
});

const bodyText = '{\n  "givenName": 5,\n  "x": "<script>alert(1)</script>"\n}';
const valueStart = bodyText.indexOf("5");

const baseReport = (
  overrides: Partial<MetadataReport> = {},
): MetadataReport => ({
  id: "r1",
  hash: "aa".repeat(32),
  url: "https://example.com/drep.json",
  effectiveUrl: "https://example.com/drep.json",
  startedAt: "2026-09-24T10:00:00Z",
  finishedAt: "2026-09-24T10:00:01Z",
  hops: [
    {
      url: "https://example.com/drep.json",
      dns: { addresses: [{ address: "93.184.216.34", family: 4 }] },
      attempts: [
        {
          address: "93.184.216.34",
          family: 4,
          outcome: "connected",
          timings: { connectMs: 12, firstByteMs: 40 },
        },
      ],
      response: {
        status: 200,
        headers: { "content-type": "application/json", "set-cookie": ["a", "b"] },
      },
    },
  ],
  body: {
    hash: "bb".repeat(32),
    size: bodyText.length,
    truncated: false,
    contentType: "application/json",
    encoding: "utf8",
    data: bodyText,
  },
  result: {
    code: "SCHEMA_INVALID",
    category: "SCHEMA_INVALID",
    message: "body.givenName must be a string",
    issues: [
      {
        field: "body.givenName",
        reason: "must be a string",
        range: {
          start: position(valueStart, 2, 16),
          end: position(valueStart + 1, 2, 17),
        },
      },
      { reason: "unknown position" },
    ],
  },
  ...overrides,
});

describe("MetadataReportView", () => {
  it("shows the category, code, message and hop details", () => {
    render(<MetadataReportView report={baseReport()} />);

    expect(screen.getByTestId("metadata-report-category")).toHaveTextContent(
      "Schema invalid",
    );
    expect(screen.getByText("SCHEMA_INVALID")).toBeInTheDocument();
    expect(
      screen.getByText("body.givenName must be a string"),
    ).toBeInTheDocument();
    const hop = screen.getByTestId("metadata-report-hop-1");
    expect(within(hop).getByText("Connected")).toBeInTheDocument();
    expect(
      within(hop).getByText("connect 12 ms, first byte 40 ms"),
    ).toBeInTheDocument();
    expect(within(hop).getByText("200")).toBeInTheDocument();
    expect(within(hop).getByText("a, b")).toBeInTheDocument();
  });

  it("renders the body as text, never as markup, with the issue highlighted", () => {
    const { container } = render(<MetadataReportView report={baseReport()} />);

    const body = screen.getByTestId("metadata-report-body");
    expect(container.querySelector("script")).toBeNull();
    expect(body).toHaveTextContent('"x": "<script>alert(1)</script>"');

    const highlights = body.querySelectorAll(".mr-hl");
    expect(highlights).toHaveLength(1);
    expect(highlights[0].textContent).toBe("5");
    // line numbers run 1..4
    expect(
      [...body.querySelectorAll(".mr-ln")].map((n) => n.textContent),
    ).toEqual(["1", "2", "3", "4"]);
  });

  it("scrolls to an issue's highlight when the issue is clicked", () => {
    render(<MetadataReportView report={baseReport()} />);

    fireEvent.click(screen.getByTestId("metadata-report-issue-0"));

    expect(scrollIntoView).toHaveBeenCalled();
    const target = scrollIntoView.mock.contexts.at(-1) as Element;
    expect(target.textContent).toBe("5");
    expect(target.className).toContain("mr-hl-active");
    // an issue without a position is listed but cannot be jumped to
    expect(
      screen.queryByTestId("metadata-report-issue-1"),
    ).not.toBeInTheDocument();
    expect(screen.getByText(/position unknown/)).toBeInTheDocument();
  });

  it("does not render binary content", () => {
    render(
      <MetadataReportView
        report={baseReport({
          body: {
            hash: "cc".repeat(32),
            size: 1234,
            truncated: false,
            encoding: "base64",
            data: "AAEC",
          },
        })}
      />,
    );

    expect(screen.getByTestId("metadata-report-binary")).toHaveTextContent(
      "Binary content, 1234 bytes",
    );
    expect(
      screen.queryByTestId("metadata-report-body"),
    ).not.toBeInTheDocument();
    expect(screen.queryByText("AAEC")).not.toBeInTheDocument();
  });

  it("says when the body was cut at the fetch limit", () => {
    const report = baseReport();
    render(
      <MetadataReportView
        report={{
          ...report,
          body: { ...report.body!, truncated: true, size: 2097152 },
          result: { ...report.result, code: "EXCEEDS_LIMIT", issues: [] },
        }}
      />,
    );

    expect(screen.getByTestId("metadata-report-truncated")).toHaveTextContent(
      "2 MB fetch limit, so the document is at least 2097152 bytes",
    );
  });

  it("shows requested and served hash for a mismatch", () => {
    const report = baseReport();
    render(
      <MetadataReportView
        report={{
          ...report,
          body: undefined,
          result: {
            code: "HASH_MISMATCH",
            category: "INVALID_CONTENT",
            message: "hash mismatch",
            servedHash: "dd".repeat(32),
            issues: [],
          },
        }}
      />,
    );

    const mismatch = screen.getByTestId("metadata-report-hash-mismatch");
    expect(within(mismatch).getByText("aa".repeat(32))).toBeInTheDocument();
    expect(within(mismatch).getByText("dd".repeat(32))).toBeInTheDocument();
    expect(
      screen.queryByText("No content was received."),
    ).not.toBeInTheDocument();
  });

  it("reports DNS errors and blocked or timed-out attempts", () => {
    render(
      <MetadataReportView
        report={baseReport({
          hops: [
            {
              url: "https://nx.example/doc.json",
              dns: { error: { code: "ENOTFOUND", message: "not found" } },
              attempts: [],
            },
            {
              url: "http://internal.example/doc.json",
              dns: { addresses: [{ address: "10.0.0.5", family: 4 }] },
              attempts: [
                {
                  address: "10.0.0.5",
                  family: 4,
                  outcome: "blocked",
                  blockedRange: "private",
                  timings: {},
                },
                {
                  address: "2001:db8::1",
                  family: 6,
                  outcome: "timeout",
                  timeoutStage: "tls",
                  errorCode: "ETIMEDOUT",
                  timings: { endMs: 5000 },
                },
              ],
            },
          ],
        })}
      />,
    );

    const first = screen.getByTestId("metadata-report-hop-1");
    expect(within(first).getByText("ENOTFOUND: not found")).toBeInTheDocument();
    expect(
      within(first).getByText("No connection was attempted."),
    ).toBeInTheDocument();
    expect(
      within(first).getByText("No HTTP response was received."),
    ).toBeInTheDocument();

    const second = screen.getByTestId("metadata-report-hop-2");
    expect(
      within(second).getByText("Blocked: not a public address (private)"),
    ).toBeInTheDocument();
    expect(
      within(second).getByText("Timed out during TLS handshake"),
    ).toBeInTheDocument();
    expect(within(second).getByText("IPv6")).toBeInTheDocument();
    expect(within(second).getByText("ETIMEDOUT")).toBeInTheDocument();
  });
});
