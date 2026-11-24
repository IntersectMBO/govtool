/**
 * Keeps an author's line breaks in Markdown: every line ends in a hard break,
 * and each extra blank line becomes an empty line rather than collapsing.
 */
export const markdownLineBreaks = (markdownText: string | number) =>
  markdownText
    .toString()
    .replace(/\r\n|\r/g, "\n")
    .replace(
      /\n\n+/g,
      (match) =>
        `\n\n${Array(match.length - 1)
          .fill("&nbsp;  \n")
          .join("")}\n`,
    )
    .split("\n")
    .map((line) => `${line}  `)
    .join("\n");
