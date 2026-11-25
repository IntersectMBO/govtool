/**
 * Writes the site-wide default preview card to the frontend's
 * public/images/og-default.png, which index.html's default og:image and the
 * home page's Organization logo point at. Run after changing the card
 * template:
 *
 *   npx ts-node -T -O '{"module":"nodenext"}' scripts/render-default-card.ts
 */
import { writeFileSync } from 'node:fs';
import { resolve } from 'node:path';

import { CardService } from '../src/seo/card/card.service';

const OUT = resolve(__dirname, '../../frontend/public/images/og-default.png');

void new CardService().renderDefault().then((png) => {
  writeFileSync(OUT, png);
  console.log(`Wrote ${OUT} (${png.length} bytes)`);
});
