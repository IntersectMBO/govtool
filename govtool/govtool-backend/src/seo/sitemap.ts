import { actionPath, isDRepIndexed } from './pages';
import { canonicalPathFor, type PageKind, type UrlScheme } from './routes';
import type { SeoAction, SeoDRep } from './seo.types';
import { escapeHtml } from './text';

const MAIN_PAGES: PageKind[] = [
  'home',
  'actionList',
  'actionHistoryList',
  'drepList',
  'forumList',
  'guides',
  'faqs',
];

/** The date part of the latest of the given instants, or null. */
const latestDate = (instants: (string | null)[]): string | null => {
  const times = instants
    .filter((t): t is string => t !== null)
    .map((t) => Date.parse(t))
    .filter((ms) => !Number.isNaN(ms));
  if (times.length === 0) return null;
  return new Date(Math.max(...times)).toISOString().slice(0, 10);
};

const url = (siteUrl: string, path: string, lastmod: string | null) =>
  `<url><loc>${escapeHtml(`${siteUrl}${path}`)}</loc>${
    lastmod === null ? '' : `<lastmod>${lastmod}</lastmod>`
  }</url>`;

/**
 * Every page worth indexing: the main pages, every governance action (a
 * finished one at its history page) and every DRep the tags index.
 */
export const buildSitemap = (
  siteUrl: string,
  scheme: UrlScheme,
  actions: SeoAction[],
  dreps: SeoDRep[],
): string => {
  const entries = [
    ...MAIN_PAGES.map((kind) =>
      url(
        siteUrl,
        canonicalPathFor(kind, undefined, scheme, false) ?? '/',
        null,
      ),
    ),
    ...actions.map((action) =>
      url(
        siteUrl,
        actionPath(action, scheme),
        latestDate([action.createdDate, ...Object.values(action.statusTimes)]),
      ),
    ),
    ...dreps
      .filter(isDRepIndexed)
      .map((drep) =>
        url(
          siteUrl,
          canonicalPathFor('drep', drep.view, scheme, false) ?? '/',
          latestDate([drep.latestRegistrationDate]),
        ),
      ),
  ];
  return [
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
    ...entries,
    '</urlset>',
  ].join('\n');
};
