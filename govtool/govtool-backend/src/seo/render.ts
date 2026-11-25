import { DEFAULT_TITLE, SITE_DESCRIPTION, SITE_NAME } from './copy';
import type { PageInfo } from './pages';
import { escapeHtml, jsonLd } from './text';

/**
 * The fragments nginx includes into index.html: tags for <head> and a text
 * summary for #root, which React replaces when it mounts. Every head element
 * carries `data-seo`, so the app can find and update the tags in place.
 */

export const DEFAULT_CARD_PATH = '/images/og-default.png';
export const DEFAULT_BODY = '';

const meta = (attr: 'name' | 'property', key: string, value: string) =>
  `<meta ${attr}="${key}" content="${escapeHtml(value)}" data-seo>`;

type Node = Record<string, unknown>;

const structuredData = (page: PageInfo, siteUrl: string): Node[] => {
  if (page.kind === 'home') {
    return [
      {
        '@type': 'WebSite',
        name: SITE_NAME,
        url: `${siteUrl}/`,
        description: page.description,
      },
      {
        '@type': 'Organization',
        name: SITE_NAME,
        url: `${siteUrl}/`,
        logo: `${siteUrl}${DEFAULT_CARD_PATH}`,
      },
    ];
  }
  const graph: Node[] = [];
  if (page.breadcrumbs.length > 0) {
    graph.push({
      '@type': 'BreadcrumbList',
      itemListElement: page.breadcrumbs.map((crumb, i) => ({
        '@type': 'ListItem',
        position: i + 1,
        name: crumb.name,
        item: `${siteUrl}${crumb.path}`,
      })),
    });
  }
  if (page.kind === 'drep' && page.drepName !== undefined) {
    graph.push({
      '@type': 'ProfilePage',
      mainEntity: {
        '@type': 'Person',
        name: page.drepName,
        description: page.description,
      },
    });
  }
  return graph;
};

/**
 * The <head> fragment. Without `siteUrl` there is nothing absolute to point
 * at, so no canonical, og:url, og:image or structured data is written.
 * `cardPath` is the page's card, e.g. /og/action/<id>/<hash>.png.
 */
export const renderHead = (
  page: PageInfo,
  siteUrl: string | null,
  cardPath: string | null,
): string => {
  const heading = page.summary.heading;
  const canonical =
    siteUrl !== null && page.canonicalPath !== null
      ? `${siteUrl}${page.canonicalPath}`
      : null;
  const tags = [
    `<title data-seo>${escapeHtml(page.title)}</title>`,
    meta('name', 'description', page.description),
  ];
  if (page.noindex) tags.push(meta('name', 'robots', 'noindex'));
  if (canonical !== null) {
    tags.push(
      `<link rel="canonical" href="${escapeHtml(canonical)}" data-seo>`,
    );
  }
  tags.push(
    meta('property', 'og:type', 'website'),
    meta('property', 'og:site_name', SITE_NAME),
    meta('property', 'og:title', heading),
    meta('property', 'og:description', page.description),
  );
  if (canonical !== null) tags.push(meta('property', 'og:url', canonical));
  if (siteUrl !== null) {
    tags.push(
      meta(
        'property',
        'og:image',
        `${siteUrl}${cardPath ?? DEFAULT_CARD_PATH}`,
      ),
      meta('property', 'og:image:width', '1200'),
      meta('property', 'og:image:height', '630'),
      meta('property', 'og:image:alt', heading),
    );
  }
  tags.push(meta('name', 'twitter:card', 'summary_large_image'));
  if (siteUrl !== null && !page.noindex) {
    const graph = structuredData(page, siteUrl);
    if (graph.length > 0) {
      tags.push(
        `<script type="application/ld+json" data-seo>${jsonLd({
          '@context': 'https://schema.org',
          '@graph': graph,
        })}</script>`,
      );
    }
  }
  return tags.join('\n');
};

/** The #root fragment: the page's heading and lines, for crawlers. */
export const renderBody = (page: PageInfo): string =>
  `<main data-seo><h1>${escapeHtml(page.summary.heading)}</h1>${page.summary.lines
    .map((line) => `<p>${escapeHtml(line)}</p>`)
    .join('')}</main>`;

/** Indexable site-wide defaults, for a page the module could not build. */
export const defaultHead = (siteUrl: string | null): string =>
  renderHead(
    {
      kind: 'unknown',
      title: DEFAULT_TITLE,
      description: SITE_DESCRIPTION,
      canonicalPath: null,
      noindex: false,
      card: null,
      breadcrumbs: [],
      summary: { heading: DEFAULT_TITLE, lines: [SITE_DESCRIPTION] },
    },
    siteUrl,
    null,
  );
