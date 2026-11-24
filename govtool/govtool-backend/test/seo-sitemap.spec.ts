import type { SeoAction, SeoDRep } from '../src/seo/seo.types';
import { buildSitemap } from '../src/seo/sitemap';
import { actionId, drepId } from './ids';

const SITE = 'https://gov.tools';
const LIVE = actionId('ab'.repeat(32), 0);
const DONE = actionId('cd'.repeat(32), 1);

const NO_STATUS = {
  ratified_epoch: null,
  enacted_epoch: null,
  dropped_epoch: null,
  expired_epoch: null,
};
const NO_TIMES = {
  ratified_time: null,
  enacted_time: null,
  dropped_time: null,
  expired_time: null,
};

const action = (overrides: Partial<SeoAction>): SeoAction => ({
  cip129Id: LIVE,
  type: 'InfoAction',
  title: 'x',
  abstract: null,
  expiryDate: null,
  expiryEpoch: null,
  createdDate: '2026-10-28T10:27:09.000Z',
  status: NO_STATUS,
  statusTimes: NO_TIMES,
  ...overrides,
});

const drep = (overrides: Partial<SeoDRep>): SeoDRep => ({
  view: drepId('a'),
  givenName: 'A',
  objectives: null,
  status: 'Active',
  votingPower: 1n,
  doNotList: false,
  metadataError: null,
  hasMetadata: true,
  latestRegistrationDate: '2025-02-10T15:55:32.000Z',
  ...overrides,
});

const locs = (xml: string) =>
  [...xml.matchAll(/<loc>([^<]*)<\/loc>/g)].map((m) => m[1]);

describe('buildSitemap', () => {
  const actions = [
    action({}),
    action({
      cip129Id: DONE,
      createdDate: '2026-04-01T00:00:00.000Z',
      status: { ...NO_STATUS, ratified_epoch: 580, enacted_epoch: 581 },
      statusTimes: {
        ...NO_TIMES,
        ratified_time: '2026-05-01T00:00:00.000Z',
        enacted_time: '2026-05-06T00:00:00.000Z',
      },
    }),
  ];
  const dreps = [
    drep({}),
    drep({ view: drepId('b'), doNotList: true }),
    drep({ view: drepId('c'), hasMetadata: false }),
    drep({ view: drepId('d'), metadataError: 'HASH_MISMATCH: x' }),
  ];

  it('lists the main pages first', () => {
    const xml = buildSitemap(SITE, 'develop', [], []);
    expect(locs(xml)).toEqual([
      `${SITE}/`,
      `${SITE}/governance_actions`,
      `${SITE}/governance_actions/history`,
      `${SITE}/drep_directory`,
      `${SITE}/proposal_discussion`,
      `${SITE}/guides`,
      `${SITE}/faqs`,
    ]);
    expect(xml.startsWith('<?xml version="1.0" encoding="UTF-8"?>')).toBe(true);
    expect(xml).toContain(
      '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    );
  });

  it('lists every action at its canonical path with its latest date', () => {
    const xml = buildSitemap(SITE, 'clean', actions, []);
    expect(xml).toContain(
      `<url><loc>${SITE}/actions/${LIVE}</loc><lastmod>2026-10-28</lastmod></url>`,
    );
    expect(xml).toContain(
      `<url><loc>${SITE}/actions/history/${DONE}</loc><lastmod>2026-05-06</lastmod></url>`,
    );
  });

  it('lists only the DReps it indexes', () => {
    const xml = buildSitemap(SITE, 'develop', [], dreps);
    const drepLocs = locs(xml).filter((l) => l.includes('/drep_directory/'));
    expect(drepLocs).toEqual([`${SITE}/drep_directory/${drepId('a')}`]);
    expect(xml).toContain('<lastmod>2025-02-10</lastmod>');
  });

  it('is balanced and escapes what it writes', () => {
    const xml = buildSitemap(
      'https://gov.tools/?a=1&b=2',
      'develop',
      actions,
      dreps,
    );
    expect(xml.match(/<url>/g)?.length).toBe(xml.match(/<\/url>/g)?.length);
    expect(xml).toContain('&amp;b=2');
    expect(xml).not.toMatch(/&(?!amp;|lt;|gt;|quot;|apos;)/);
  });
});
