import {
  escapeHtml,
  jsonLd,
  plainText,
  stripEmoji,
  truncate,
} from '../src/seo/text';

describe('plainText', () => {
  it('strips Markdown, HTML, control and bidi characters', () => {
    expect(plainText('# Hello **world** <b>x</b>\u202E\u0007  y')).toBe(
      'Hello world x y',
    );
  });

  it('keeps link text and inline code text', () => {
    expect(plainText('[link](https://a.b) and `code`')).toBe('link and code');
  });

  it('removes isolate controls and collapses newlines', () => {
    expect(plainText('a\u2066b\u2069\n\n  c')).toBe('ab c');
  });

  it('turns null and undefined into an empty string', () => {
    expect(plainText(null)).toBe('');
    expect(plainText(undefined)).toBe('');
  });
});

describe('stripEmoji', () => {
  it('removes emoji with skin tones and variation selectors', () => {
    expect(stripEmoji('Vote 🗳️ now 👍🏽')).toBe('Vote  now ');
  });

  it('removes ZWJ sequences and keycaps', () => {
    expect(stripEmoji('a👨‍👩‍👧b1️⃣c')).toBe('ab1c');
  });

  it('leaves Japanese text alone', () => {
    expect(stripEmoji('日本コミュニティ')).toBe('日本コミュニティ');
  });
});

describe('truncate', () => {
  it('cuts at a word boundary and appends an ellipsis', () => {
    expect(truncate('one two three four', 12)).toBe('one two…');
  });

  it('leaves short text alone', () => {
    expect(truncate('short', 155)).toBe('short');
  });

  it('cuts a single long word to exactly the limit', () => {
    const out = truncate('x'.repeat(300), 155);
    expect(out).toHaveLength(155);
    expect(out.endsWith('…')).toBe(true);
  });
});

describe('escapeHtml', () => {
  it('leaves no markup characters', () => {
    const out = escapeHtml('"></title><script>alert(1)</script>\'');
    expect(out).not.toMatch(/[<>"']/);
    expect(out).toContain('&lt;script&gt;');
  });

  it('escapes ampersands first', () => {
    expect(escapeHtml('a & <b>')).toBe('a &amp; &lt;b&gt;');
  });
});

describe('jsonLd', () => {
  it('cannot close its script element', () => {
    const out = jsonLd({ name: '</script><script>alert(1)</script>' });
    expect(out).not.toContain('</script>');
    expect(out).not.toMatch(/[<>&]/);
    expect(JSON.parse(out)).toEqual({
      name: '</script><script>alert(1)</script>',
    });
  });

  it('escapes line and paragraph separators', () => {
    const out = jsonLd({ a: '\u2028\u2029' });
    expect(out).toContain('\\u2028');
    expect(out).toContain('\\u2029');
  });
});

describe('plainText on hostile input', () => {
  it.each([
    ['a run of <', '<'.repeat(200_000)],
    ['a run of [', '['.repeat(200_000)],
    ['unclosed links', '[a]('.repeat(50_000)],
    ['a run of newlines', '\n'.repeat(200_000)],
    ['headings over newlines', '#\n'.repeat(100_000)],
    ['numbered lines without a dot', '1'.repeat(100) + '\n'.repeat(2_000)],
  ])('stays fast on %s', (_, input) => {
    const start = performance.now();
    plainText(input);
    expect(performance.now() - start).toBeLessThan(50);
  });

  it('reads only the start of a very long text', () => {
    expect(plainText('word '.repeat(10_000)).length).toBeLessThanOrEqual(4_000);
  });
});
