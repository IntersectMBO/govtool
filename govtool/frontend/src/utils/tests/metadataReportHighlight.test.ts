import type { ContentIssue } from "@models";
import {
  buildHighlightedLines,
  normalizeIssueRanges,
  segmentText,
  splitSegmentsIntoLines,
} from "../metadataReportHighlight";

const issue = (start: number, end: number, reason = "bad"): ContentIssue => ({
  reason,
  range: {
    start: { offset: start, byteOffset: start, line: 1, column: start + 1 },
    end: { offset: end, byteOffset: end, line: 1, column: end + 1 },
  },
});

const joined = (text: string, issues: ContentIssue[]) =>
  segmentText(text, normalizeIssueRanges(text, issues))
    .map((s) => s.text)
    .join("");

describe("normalizeIssueRanges", () => {
  it("keeps a valid half-open range", () => {
    expect(normalizeIssueRanges("abcdef", [issue(1, 3)])).toEqual([
      { id: 0, start: 1, end: 3 },
    ]);
  });

  it("skips issues without a range or with non-numeric offsets", () => {
    const noRange: ContentIssue = { reason: "missing" };
    const nan = issue(Number.NaN, 2);
    expect(normalizeIssueRanges("abc", [noRange, nan])).toEqual([]);
  });

  it("clamps an end past the text and a negative start", () => {
    expect(normalizeIssueRanges("abc", [issue(-4, 99)])).toEqual([
      { id: 0, start: 0, end: 3 },
    ]);
  });

  it("skips a range that starts past the end", () => {
    expect(normalizeIssueRanges("abc", [issue(10, 12)])).toEqual([]);
  });

  it("widens a zero-width range to one character", () => {
    expect(normalizeIssueRanges("abc", [issue(1, 1)])).toEqual([
      { id: 0, start: 1, end: 2 },
    ]);
    // "parsing stopped at end of input" points at the last character
    expect(normalizeIssueRanges("abc", [issue(3, 3)])).toEqual([
      { id: 0, start: 2, end: 3 },
    ]);
  });

  it("swaps a reversed range", () => {
    expect(normalizeIssueRanges("abcdef", [issue(4, 2)])).toEqual([
      { id: 0, start: 2, end: 4 },
    ]);
  });

  it("never splits a surrogate pair", () => {
    const text = "a\u{1F600}b"; // a, high, low, b
    expect(normalizeIssueRanges(text, [issue(2, 3)])).toEqual([
      { id: 0, start: 1, end: 3 },
    ]);
    expect(normalizeIssueRanges(text, [issue(0, 2)])).toEqual([
      { id: 0, start: 0, end: 3 },
    ]);
  });

  it("returns nothing for empty text", () => {
    expect(normalizeIssueRanges("", [issue(0, 1)])).toEqual([]);
  });
});

describe("segmentText", () => {
  it("returns the whole text as one plain segment without ranges", () => {
    expect(segmentText("hello", [])).toEqual([
      { text: "hello", start: 0, issueIds: [], firstOfIssueIds: [] },
    ]);
  });

  it("splits around a range", () => {
    const segments = segmentText("abcdef", [{ id: 0, start: 2, end: 4 }]);
    expect(segments.map((s) => [s.text, s.issueIds])).toEqual([
      ["ab", []],
      ["cd", [0]],
      ["ef", []],
    ]);
  });

  it("covers the overlap with both issues and marks each first segment", () => {
    const segments = segmentText("abcdefgh", [
      { id: 0, start: 1, end: 5 },
      { id: 1, start: 3, end: 7 },
    ]);
    expect(
      segments.map((s) => [s.text, s.issueIds, s.firstOfIssueIds]),
    ).toEqual([
      ["a", [], []],
      ["bc", [0], [0]],
      ["de", [0, 1], [1]],
      ["fg", [1], []],
      ["h", [], []],
    ]);
  });

  it("handles a range nested in another and identical ranges", () => {
    const segments = segmentText("abcdef", [
      { id: 0, start: 0, end: 6 },
      { id: 1, start: 2, end: 3 },
      { id: 2, start: 2, end: 3 },
    ]);
    expect(segments.map((s) => [s.text, s.issueIds])).toEqual([
      ["ab", [0]],
      ["c", [0, 1, 2]],
      ["def", [0]],
    ]);
  });

  it("always reassembles to the original text", () => {
    const text = '{\n  "a": 1,\n  "b": \u{1F600}\n}';
    expect(
      joined(text, [issue(3, 9), issue(5, 20), issue(-1, 2), issue(40, 50)]),
    ).toBe(text);
  });
});

describe("splitSegmentsIntoLines", () => {
  it("numbers lines and drops the newline characters", () => {
    const lines = splitSegmentsIntoLines(segmentText("ab\ncd\n", []));
    expect(lines.map((l) => [l.lineNumber, l.segments.map((s) => s.text)]))
      .toEqual([
        [1, ["ab"]],
        [2, ["cd"]],
        [3, []],
      ]);
  });

  it("splits a highlight that spans lines and marks only its first piece", () => {
    const lines = splitSegmentsIntoLines(
      segmentText("ab\ncd", [{ id: 0, start: 1, end: 4 }]),
    );
    expect(
      lines.map((l) =>
        l.segments.map((s) => [s.text, s.start, s.issueIds, s.firstOfIssueIds]),
      ),
    ).toEqual([
      [
        ["a", 0, [], []],
        ["b", 1, [0], [0]],
      ],
      [
        ["c", 3, [0], []],
        ["d", 4, [], []],
      ],
    ]);
  });

  it("keeps a visible marker for a highlight covering only a line break", () => {
    const lines = splitSegmentsIntoLines(
      segmentText("ab\ncd", [{ id: 0, start: 2, end: 3 }]),
    );
    expect(lines[0].segments).toEqual([
      { text: "ab", start: 0, issueIds: [], firstOfIssueIds: [] },
      { text: "", start: 2, issueIds: [0], firstOfIssueIds: [0] },
    ]);
    expect(lines[1].segments.map((s) => s.text)).toEqual(["cd"]);
  });
});

describe("buildHighlightedLines", () => {
  it("reports the line each issue starts on", () => {
    const text = '{\n  "givenName": 5\n}';
    const start = text.indexOf("5");
    const { issueLines, lines } = buildHighlightedLines(text, [
      issue(start, start + 1),
      { reason: "no position" },
    ]);
    expect(lines).toHaveLength(3);
    expect(issueLines.get(0)).toBe(2);
    expect(issueLines.has(1)).toBe(false);
  });
});
