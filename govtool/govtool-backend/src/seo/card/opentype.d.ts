/**
 * The part of Satori's own font parser the card renderer uses: a font's
 * character map, to tell which characters the bundled fonts can draw.
 */
declare module '@shuding/opentype.js' {
  export function parse(buffer: ArrayBufferLike): {
    tables: { cmap: { glyphIndexMap: Record<string, number> } };
  };
}
