import type { ContentIssue } from "@models";

/** An issue's range, clamped to the text. `id` is the issue's index. */
export type HighlightRange = { id: number; start: number; end: number };

export type HighlightSegment = {
  text: string;
  /** UTF-16 offset of the segment's first code unit in the whole text. */
  start: number;
  /** The issues whose range covers this segment, in ascending id order. */
  issueIds: number[];
  /** The issues for which this is the first covering segment (scroll target). */
  firstOfIssueIds: number[];
};

export type HighlightLine = {
  /** 1-based. */
  lineNumber: number;
  segments: HighlightSegment[];
};

const isHighSurrogate = (code: number) => code >= 0xd800 && code <= 0xdbff;
const isLowSurrogate = (code: number) => code >= 0xdc00 && code <= 0xdfff;

/** True when `index` falls between the two halves of a surrogate pair. */
const splitsPair = (text: string, index: number) =>
  index > 0 &&
  index < text.length &&
  isHighSurrogate(text.charCodeAt(index - 1)) &&
  isLowSurrogate(text.charCodeAt(index));

const toIndex = (value: unknown): number | null => {
  if (typeof value !== "number" || !Number.isFinite(value)) return null;
  return Math.floor(value);
};

/**
 * Turns issue ranges into highlightable ranges over `text`. Offsets are UTF-16
 * code units (spec §2.8), ranges half-open. Issues without a range, or with a
 * non-numeric one, are skipped. Out-of-bounds ranges are clamped; a range that
 * starts past the end is skipped. A zero-width range (such as "parsing stopped
 * here") widens to one character so it stays visible. A range never splits a
 * surrogate pair: it widens to cover the whole character.
 */
export const normalizeIssueRanges = (
  text: string,
  issues: ContentIssue[],
): HighlightRange[] => {
  const { length } = text;
  const ranges: HighlightRange[] = [];

  issues.forEach((issue, id) => {
    const rawStart = toIndex(issue.range?.start?.offset);
    const rawEnd = toIndex(issue.range?.end?.offset);
    if (rawStart === null || rawEnd === null || length === 0) return;
    if (rawStart > length) return;

    let start = Math.max(0, Math.min(rawStart, rawEnd));
    let end = Math.min(length, Math.max(rawStart, rawEnd));

    if (end === start) {
      if (start < length) end = start + 1;
      else start = length - 1;
    }
    if (splitsPair(text, start)) start -= 1;
    if (splitsPair(text, end)) end += 1;

    ranges.push({ id, start, end });
  });

  return ranges;
};

/**
 * Splits `text` at every range boundary. Each segment lists every range that
 * covers it, so overlapping ranges produce a segment covered by both rather
 * than one hiding the other. Concatenating the segments' text gives `text`.
 */
export const segmentText = (
  text: string,
  ranges: HighlightRange[],
): HighlightSegment[] => {
  if (text.length === 0) return [];

  const boundaries = new Set<number>([0, text.length]);
  ranges.forEach(({ start, end }) => {
    boundaries.add(start);
    boundaries.add(end);
  });
  const points = [...boundaries].sort((a, b) => a - b);

  const seen = new Set<number>();
  const segments: HighlightSegment[] = [];

  for (let i = 0; i < points.length - 1; i++) {
    const start = points[i];
    const end = points[i + 1];
    const issueIds = ranges
      .filter((range) => range.start <= start && range.end >= end)
      .map((range) => range.id)
      .sort((a, b) => a - b);
    const firstOfIssueIds = issueIds.filter((id) => !seen.has(id));
    firstOfIssueIds.forEach((id) => seen.add(id));

    segments.push({
      text: text.slice(start, end),
      start,
      issueIds,
      firstOfIssueIds,
    });
  }

  return segments;
};

/**
 * Breaks segments into display lines on `\n`. The newline itself is not
 * rendered. A highlighted segment made of nothing but line breaks keeps one
 * empty piece, so it still has something to show and scroll to.
 */
export const splitSegmentsIntoLines = (
  segments: HighlightSegment[],
): HighlightLine[] => {
  const lines: HighlightLine[] = [{ lineNumber: 1, segments: [] }];

  segments.forEach((segment) => {
    const pieces = segment.text.split("\n");
    let offset = segment.start;
    let firstEmitted = false;
    const hasText = pieces.some((piece) => piece.length > 0);

    pieces.forEach((piece, index) => {
      if (index > 0) {
        lines.push({ lineNumber: lines.length + 1, segments: [] });
      }
      const keepEmpty =
        index === 0 && !hasText && segment.issueIds.length > 0;

      if (piece.length > 0 || keepEmpty) {
        lines[lines.length - 1].segments.push({
          text: piece,
          start: offset,
          issueIds: segment.issueIds,
          firstOfIssueIds: firstEmitted ? [] : segment.firstOfIssueIds,
        });
        firstEmitted = true;
      }
      offset += piece.length + 1;
    });
  });

  return lines;
};

/** The line each issue's highlight starts on, keyed by issue id. */
export const issueStartLines = (lines: HighlightLine[]) => {
  const result = new Map<number, number>();
  lines.forEach(({ lineNumber, segments }) =>
    segments.forEach(({ firstOfIssueIds }) =>
      firstOfIssueIds.forEach((id) => {
        if (!result.has(id)) result.set(id, lineNumber);
      }),
    ),
  );
  return result;
};

export const buildHighlightedLines = (text: string, issues: ContentIssue[]) => {
  const lines = splitSegmentsIntoLines(
    segmentText(text, normalizeIssueRanges(text, issues)),
  );
  return { lines, issueLines: issueStartLines(lines) };
};
