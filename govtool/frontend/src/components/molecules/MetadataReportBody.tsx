import { useEffect, useMemo, useRef, useState } from "react";
import { Box, ButtonBase } from "@mui/material";

import { Button, Typography } from "@atoms";
import { useTranslation } from "@hooks";
import {
  ContentIssue,
  METADATA_FETCH_LIMIT_BYTES,
  ReportBody,
} from "@models";
import { buildHighlightedLines } from "@utils";

/** Past this many lines the body renders on request, to keep the page light. */
const INITIAL_LINE_LIMIT = 2000;

const formatMegabytes = (bytes: number) =>
  `${Math.round((bytes / (1024 * 1024)) * 10) / 10} MB`;

type MetadataReportBodyProps = {
  body?: ReportBody;
  issues: ContentIssue[];
};

/**
 * The content a url served, with every issue's range highlighted. The body is
 * arbitrary remote data: it is only ever rendered as React text nodes, never as
 * HTML, and nothing in it becomes a link.
 */
export const MetadataReportBody = ({ body, issues }: MetadataReportBodyProps) => {
  const { t } = useTranslation();
  const containerRef = useRef<HTMLDivElement>(null);
  const [showAllLines, setShowAllLines] = useState(false);
  const [activeIssue, setActiveIssue] = useState<number | null>(null);
  const [pendingScroll, setPendingScroll] = useState<number | null>(null);

  const isText = body?.encoding === "utf8";
  const { lines, issueLines } = useMemo(
    () => {
      if (!isText || !body) {
        return { lines: [], issueLines: new Map<number, number>() };
      }
      return buildHighlightedLines(body.data, issues);
    },
    [isText, body?.data, issues],
  );

  const visibleLines =
    showAllLines || lines.length <= INITIAL_LINE_LIMIT
      ? lines
      : lines.slice(0, INITIAL_LINE_LIMIT);

  useEffect(() => {
    if (pendingScroll === null) return;
    const target = containerRef.current?.querySelector(
      `[data-issue-anchors~="${pendingScroll}"]`,
    );
    target?.scrollIntoView?.({ block: "center", behavior: "smooth" });
    setPendingScroll(null);
  }, [pendingScroll, showAllLines]);

  const jumpToIssue = (id: number) => {
    const line = issueLines.get(id);
    if (line === undefined) return;
    if (line > visibleLines.length) setShowAllLines(true);
    setActiveIssue(id);
    setPendingScroll(id);
  };

  return (
    <Box sx={{ display: "flex", flexDirection: "column", gap: 1.5 }}>
      <Typography variant="body2" fontWeight={600}>
        {t("metadataReport.body.title")}
      </Typography>

      {!body && (
        <Typography variant="body2">{t("metadataReport.body.none")}</Typography>
      )}

      {body && (
        <Typography variant="caption" color="neutralGray">
          {body.contentType
            ? t("metadataReport.body.metaWithType", {
                size: body.size,
                contentType: body.contentType,
              })
            : t("metadataReport.body.meta", { size: body.size })}
        </Typography>
      )}

      {body?.truncated && (
        <Typography
          variant="body2"
          sx={{ color: "errorRed" }}
          data-testid="metadata-report-truncated"
        >
          {t("metadataReport.body.truncated", {
            limit: formatMegabytes(METADATA_FETCH_LIMIT_BYTES),
            size: body.size,
          })}
        </Typography>
      )}

      {body && !isText && (
        <Typography variant="body2" data-testid="metadata-report-binary">
          {t("metadataReport.body.binary", { size: body.size })}
        </Typography>
      )}

      {isText && (
        <Box
          ref={containerRef}
          data-testid="metadata-report-body"
          sx={{
            maxHeight: 420,
            overflow: "auto",
            border: "1px solid #D6D8FF",
            borderRadius: 1,
            bgcolor: "#FBFBFF",
            fontFamily: "monospace",
            fontSize: 12,
            lineHeight: "18px",
            "& .mr-line": { display: "flex", minWidth: "fit-content" },
            "& .mr-ln": {
              flex: "0 0 auto",
              minWidth: "3.5em",
              pr: 1.5,
              textAlign: "right",
              color: "#8E908E",
              userSelect: "none",
              borderRight: "1px solid #E6E6F0",
              mr: 1,
            },
            "& .mr-text": { whiteSpace: "pre", pr: 1 },
            "& .mr-hl": {
              bgcolor: "rgba(229, 57, 53, 0.18)",
              borderBottom: "2px solid rgba(229, 57, 53, 0.7)",
            },
            "& .mr-hl-multi": { bgcolor: "rgba(229, 57, 53, 0.32)" },
            "& .mr-hl-empty": {
              display: "inline-block",
              width: "0.6em",
            },
            "& .mr-hl-active": { outline: "2px solid #2F62DC" },
          }}
        >
          {visibleLines.map((line) => (
            <div className="mr-line" key={line.lineNumber}>
              <span className="mr-ln">{line.lineNumber}</span>
              <span className="mr-text">
                {line.segments.map((segment) => {
                  if (segment.issueIds.length === 0) {
                    return <span key={segment.start}>{segment.text}</span>;
                  }
                  const classes = [
                    "mr-hl",
                    segment.issueIds.length > 1 ? "mr-hl-multi" : "",
                    segment.text.length === 0 ? "mr-hl-empty" : "",
                    activeIssue !== null &&
                    segment.issueIds.includes(activeIssue)
                      ? "mr-hl-active"
                      : "",
                  ]
                    .filter(Boolean)
                    .join(" ");
                  return (
                    <span
                      key={segment.start}
                      className={classes}
                      data-issue-anchors={
                        segment.firstOfIssueIds.length > 0
                          ? segment.firstOfIssueIds.join(" ")
                          : undefined
                      }
                      title={segment.issueIds
                        .map((id) => issues[id]?.reason)
                        .filter(Boolean)
                        .join("\n")}
                    >
                      {segment.text}
                    </span>
                  );
                })}
              </span>
            </div>
          ))}
        </Box>
      )}

      {isText && visibleLines.length < lines.length && (
        <Box sx={{ display: "flex", alignItems: "center", gap: 2 }}>
          <Typography variant="caption" color="neutralGray">
            {t("metadataReport.body.hiddenLines", {
              count: lines.length - visibleLines.length,
            })}
          </Typography>
          <Button
            size="small"
            variant="text"
            onClick={() => setShowAllLines(true)}
          >
            {t("metadataReport.body.showAllLines", { count: lines.length })}
          </Button>
        </Box>
      )}

      {issues.length > 0 && (
        <Box sx={{ display: "flex", flexDirection: "column", gap: 0.5 }}>
          <Typography variant="body2" fontWeight={600}>
            {t("metadataReport.issues.title")}
          </Typography>
          <Box
            component="ul"
            sx={{ m: 0, pl: 2.5, display: "flex", flexDirection: "column" }}
            data-testid="metadata-report-issues"
          >
            {issues.map((issue, id) => {
              const canJump = issueLines.has(id);
              const position = issue.range
                ? t("metadataReport.issues.position", {
                    line: issue.range.start.line,
                    column: issue.range.start.column,
                  })
                : t("metadataReport.issues.noPosition");
              const label = (
                <>
                  {issue.field && (
                    <Box
                      component="span"
                      sx={{ fontFamily: "monospace", fontWeight: 600 }}
                    >
                      {issue.field}
                      {": "}
                    </Box>
                  )}
                  {issue.reason}
                  <Box component="span" sx={{ color: "#8E908E" }}>
                    {` (${position})`}
                  </Box>
                </>
              );
              return (
                // eslint-disable-next-line react/no-array-index-key
                <li key={id}>
                  {canJump ? (
                    <ButtonBase
                      onClick={() => jumpToIssue(id)}
                      title={t("metadataReport.issues.jumpTo")}
                      data-testid={`metadata-report-issue-${id}`}
                      sx={{
                        textAlign: "left",
                        fontSize: 14,
                        fontFamily: "Poppins",
                        color: "primary.main",
                        textDecoration: "underline",
                      }}
                    >
                      <span>{label}</span>
                    </ButtonBase>
                  ) : (
                    <Typography variant="body2" component="span">
                      {label}
                    </Typography>
                  )}
                </li>
              );
            })}
          </Box>
        </Box>
      )}
    </Box>
  );
};
