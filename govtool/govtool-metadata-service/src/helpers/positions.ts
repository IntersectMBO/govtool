import { findNodeAtLocation, parseTree, type Node, type ParseError } from "jsonc-parser";

/**
 * Source positions for fetch-report issues. `offset` counts UTF-16 code units
 * in the decoded text, so a JS consumer can slice the string directly;
 * `byteOffset` counts UTF-8 bytes; `line` and `column` are 1-based, with
 * `column` counted in UTF-16 code units from the line start.
 */
export interface SourcePosition {
  offset: number;
  byteOffset: number;
  line: number;
  column: number;
}

export interface SourceRange {
  start: SourcePosition;
  end: SourcePosition;
}

export class PositionIndex {
  private readonly lineStarts: number[] = [0];

  constructor(private readonly text: string) {
    for (let i = 0; i < text.length; i++) {
      if (text.charCodeAt(i) === 10) this.lineStarts.push(i + 1);
    }
  }

  at(offset: number): SourcePosition {
    const clamped = Math.max(0, Math.min(offset, this.text.length));
    let lo = 0;
    let hi = this.lineStarts.length - 1;
    while (lo < hi) {
      const mid = (lo + hi + 1) >> 1;
      if (this.lineStarts[mid] <= clamped) lo = mid;
      else hi = mid - 1;
    }
    return {
      offset: clamped,
      byteOffset: Buffer.byteLength(this.text.slice(0, clamped), "utf8"),
      line: lo + 1,
      column: clamped - this.lineStarts[lo] + 1,
    };
  }

  range(start: number, end: number): SourceRange {
    return { start: this.at(start), end: this.at(Math.max(start, end)) };
  }

  /**
   * From a parser's 1-based line and column, where the column counts code
   * points (as JSON5 does). Returns a one-character range, or the end of the
   * text when the parser stopped there.
   */
  rangeAtLineColumn(line: number, column: number): SourceRange | undefined {
    if (!Number.isInteger(line) || !Number.isInteger(column) || line < 1 || column < 1) {
      return undefined;
    }
    if (line > this.lineStarts.length) return undefined;
    let offset = this.lineStarts[line - 1];
    for (let c = 1; c < column && offset < this.text.length; c++) {
      const code = this.text.codePointAt(offset)!;
      if (code === 10) break;
      offset += code > 0xffff ? 2 : 1;
    }
    if (offset >= this.text.length) return this.range(this.text.length, this.text.length);
    const code = this.text.codePointAt(offset)!;
    return this.range(offset, offset + (code > 0xffff ? 2 : 1));
  }

  /**
   * The range of the value at a JSON path, such as `["body", "givenName"]`.
   * Undefined when the text is not parseable as JSON with comments and
   * trailing commas (some JSON5 is not), or the path is absent.
   */
  rangeOfPath(path: (string | number)[]): SourceRange | undefined {
    const tree = this.tree();
    if (!tree) return undefined;
    const node = findNodeAtLocation(tree, path);
    if (!node) return undefined;
    return this.range(node.offset, node.offset + node.length);
  }

  private cachedTree?: Node | null;

  private tree(): Node | undefined {
    if (this.cachedTree === undefined) {
      const errors: ParseError[] = [];
      const tree = parseTree(this.text, errors, { allowTrailingComma: true, disallowComments: false });
      this.cachedTree = errors.length === 0 && tree ? tree : null;
    }
    return this.cachedTree ?? undefined;
  }
}
