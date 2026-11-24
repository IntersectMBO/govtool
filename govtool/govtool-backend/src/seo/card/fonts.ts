import { readFileSync } from 'node:fs';
import { join, resolve } from 'node:path';

import { parse } from '@shuding/opentype.js';
import type { Font } from 'satori';

/**
 * The card's fonts and images, read once from assets/seo. The source and the
 * build sit at the same depth (src/seo/card, dist/seo/card), and the image
 * keeps assets/ beside dist/, so one relative path serves both.
 */
export const ASSETS_DIR = resolve(__dirname, '../../../assets/seo');

const FONT_FILES = [
  { file: 'Poppins-Regular.ttf', name: 'Poppins', weight: 400 },
  { file: 'Poppins-Bold.ttf', name: 'Poppins', weight: 700 },
  { file: 'NotoSansJP-Regular.otf', name: 'Noto Sans JP', weight: 400 },
  { file: 'NotoSansJP-Bold.otf', name: 'Noto Sans JP', weight: 700 },
] as const;

const read = (file: string): Buffer =>
  readFileSync(join(ASSETS_DIR, 'fonts', file));

let fonts: Font[] | undefined;
let covered: Set<number> | undefined;

/**
 * Every bundled font, in one array that is kept: Satori caches its parsed
 * fonts per array, and falls back from Poppins to Noto Sans JP for the
 * characters Poppins lacks. No per-render font loading.
 */
export const cardFonts = (): Font[] =>
  (fonts ??= FONT_FILES.map(({ file, name, weight }) => ({
    name,
    data: read(file),
    weight,
    style: 'normal' as const,
  })));

/**
 * The code points the bundled fonts can draw, read from their character
 * maps once. A card whose text has any other character gets the default
 * card instead of empty boxes.
 */
export const coveredCodePoints = (): Set<number> => {
  if (covered === undefined) {
    covered = new Set();
    for (const { file } of FONT_FILES) {
      const data = read(file);
      const font = parse(
        data.buffer.slice(data.byteOffset, data.byteOffset + data.byteLength),
      );
      for (const codePoint of Object.keys(font.tables.cmap.glyphIndexMap)) {
        covered.add(Number(codePoint));
      }
    }
  }
  return covered;
};

/** A bundled image as a data URI, so Satori never fetches a URL. */
export const assetDataUri = (file: string, mime: string): string =>
  `data:${mime};base64,${readFileSync(join(ASSETS_DIR, file)).toString('base64')}`;
