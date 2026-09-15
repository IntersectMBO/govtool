import { removeMarkdown } from "../removeMarkdown";

describe("removeMarkdown", () => {
  it("should remove bold markdown", () => {
    const markdown = "**Hello** World";
    const expected = "Hello World";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove italic markdown", () => {
    const markdown = "*Hello* World";
    const expected = "Hello World";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove strikethrough markdown", () => {
    const markdown = "~~Hello~~ World";
    const expected = "Hello World";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove image markdown", () => {
    const markdown = "![Alt Text](image.jpg)";
    const expected = "";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove link markdown", () => {
    const markdown = "[Link Text](https://example.com)";
    const expected = "Link Text";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove inline code markdown", () => {
    const markdown = "`code`";
    const expected = "code";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove blockquote markdown", () => {
    const markdown = "> Blockquote";
    const expected = "Blockquote";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove heading markdown", () => {
    const markdown = "# Heading";
    const expected = "Heading";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });

  it("should remove multiple newlines", () => {
    const markdown = "Hello\n\n\nWorld";
    const expected = "Hello\nWorld";
    const result = removeMarkdown(markdown);
    expect(result).toEqual(expected);
  });
});
