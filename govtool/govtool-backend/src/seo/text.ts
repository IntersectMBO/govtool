/**
 * Turning user-written metadata (titles, abstracts, names, objectives) into
 * text that is safe to put in HTML, attributes, JSON-LD and preview cards.
 * Anyone can publish an anchor, so every value is treated as hostile.
 */

const BIDI_CONTROLS = /[\u202A-\u202E\u2066-\u2069]/g;

// C0 and C1 control characters, newlines and tabs included, become a space,
// so "a<newline>b" stays two words. Filtered by code point, since lint bans
// control characters in a regular expression.
const isControl = (code: number): boolean =>
  code < 0x20 || (code >= 0x7f && code <= 0x9f);
const controlsToSpaces = (s: string): string =>
  Array.from(s, (c) => (isControl(c.charCodeAt(0)) ? ' ' : c)).join('');

/**
 * The most of a text that is read. Every caller keeps 155 characters or
 * fewer, and documents can be megabytes: capping first bounds the work.
 */
const MAX_INPUT = 4_000;

/**
 * Markdown and HTML reduced to their text, on one line. Every pattern is
 * linear: no class can run past the delimiter that opens it, and the
 * line-start patterns match spaces, never newlines, so hostile input such as
 * a long run of "<" or "[" cannot make a pattern rescan the text.
 */
export const plainText = (input: string | null | undefined): string => {
  if (input === null || input === undefined) return '';
  const text = input
    .slice(0, MAX_INPUT)
    .replace(/<[^<>]*>/g, ' ')
    .replace(/```[^\n`]*/g, ' ')
    .replace(/!\[([^[\]]*)\]\(([^()]*)\)/g, '$1')
    .replace(/\[([^[\]]*)\]\(([^()]*)\)/g, '$1')
    .replace(/`([^`]*)`/g, '$1')
    .replace(/^[ \t]{0,3}#{1,6}[ \t]+/gm, '')
    .replace(/^[ \t]{0,3}>[ \t]?/gm, '')
    .replace(/^[ \t]{0,8}(?:[-*+]|\d{1,9}[.)])[ \t]+/gm, '')
    .replace(/(\*\*|__|~~)/g, '')
    .replace(
      /(^|[^\p{L}\p{N}])[*_]([^*_\s][^*_]*?)[*_](?=[^\p{L}\p{N}]|$)/gu,
      '$1$2',
    );

  return controlsToSpaces(text.replace(BIDI_CONTROLS, ''))
    .replace(/\s+/g, ' ')
    .trim();
};

/**
 * Emoji removed, for card text: Satori draws nothing for them without
 * bundled emoji images. Keycap bases (digits, # and *) stay.
 */
export const stripEmoji = (s: string): string =>
  s.replace(
    /\p{Extended_Pictographic}|\p{Emoji_Modifier}|[\u{1F1E6}-\u{1F1FF}]|\u200D|\uFE0F|\u20E3/gu,
    '',
  );

/**
 * At most `max` characters. A cut ends at the last space in the second half
 * of the allowance, or mid-word when there is none, and is marked with "…".
 */
export const truncate = (s: string, max: number): string => {
  if (s.length <= max) return s;
  let cut = s.slice(0, max - 1);
  // Never leave half of a surrogate pair.
  if (/[\uD800-\uDBFF]$/.test(cut)) cut = cut.slice(0, -1);
  const space = cut.lastIndexOf(' ');
  if (space > max / 2) cut = cut.slice(0, space).trimEnd();
  return `${cut}…`;
};

const HTML_ESCAPES: Record<string, string> = {
  '&': '&amp;',
  '<': '&lt;',
  '>': '&gt;',
  '"': '&quot;',
  "'": '&#39;',
};

/** Safe in element content and in quoted attribute values. */
export const escapeHtml = (s: string): string =>
  s.replace(/[&<>"']/g, (c) => HTML_ESCAPES[c]);

const LINE_SEPARATOR = String.fromCharCode(0x2028);
const PARAGRAPH_SEPARATOR = String.fromCharCode(0x2029);

/**
 * JSON for a `<script type="application/ld+json">` element. `<`, `>` and `&`
 * are escaped so the text cannot close the element, and U+2028/U+2029 so
 * older parsers do not read them as line ends.
 */
export const jsonLd = (value: unknown): string =>
  JSON.stringify(value)
    .replace(/</g, '\\u003c')
    .replace(/>/g, '\\u003e')
    .replace(/&/g, '\\u0026')
    .split(LINE_SEPARATOR)
    .join('\\u2028')
    .split(PARAGRAPH_SEPARATOR)
    .join('\\u2029');
