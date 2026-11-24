import type { PageInfo } from '../src/seo/pages';
import {
  DEFAULT_BODY,
  defaultHead,
  renderBody,
  renderHead,
} from '../src/seo/render';
import { actionId } from './ids';

const SITE = 'https://gov.tools';
const ACTION = actionId('ab'.repeat(32), 0);
const CARD = `/og/action/${ACTION}/0123456789ab.png`;
const EVIL = '"></title><script>alert(1)</script>\u202E';

function actionPage(overrides: Partial<PageInfo> = {}): PageInfo {
  return {
    kind: 'action',
    title: 'Fund the tooling | Cardano GovTool',
    description: 'Info action · voting open until 30 Nov 2026. A plan.',
    canonicalPath: `/governance_actions/${ACTION}`,
    noindex: false,
    card: {
      kind: 'action',
      id: ACTION,
      title: 'Fund the tooling',
      label: 'Info action · Voting open',
    },
    breadcrumbs: [
      { name: 'Home', path: '/' },
      { name: 'Governance actions', path: '/governance_actions' },
      { name: 'Fund the tooling', path: `/governance_actions/${ACTION}` },
    ],
    summary: { heading: 'Fund the tooling', lines: ['A plan.'] },
    ...overrides,
  };
}

const jsonLdOf = (head: string): unknown => {
  const match =
    /<script type="application\/ld\+json" data-seo>(.*?)<\/script>/s.exec(head);
  return match === null ? undefined : JSON.parse(match[1]);
};

describe('renderHead', () => {
  it('writes the tags in order with absolute URLs', () => {
    const head = renderHead(actionPage(), SITE, CARD);
    const order = [
      '<title data-seo>Fund the tooling | Cardano GovTool</title>',
      '<meta name="description" content="Info action · voting open until 30 Nov 2026. A plan." data-seo>',
      `<link rel="canonical" href="${SITE}/governance_actions/${ACTION}" data-seo>`,
      '<meta property="og:type" content="website" data-seo>',
      '<meta property="og:site_name" content="Cardano GovTool" data-seo>',
      '<meta property="og:title" content="Fund the tooling" data-seo>',
      '<meta property="og:description"',
      `<meta property="og:url" content="${SITE}/governance_actions/${ACTION}" data-seo>`,
      `<meta property="og:image" content="${SITE}${CARD}" data-seo>`,
      '<meta property="og:image:width" content="1200" data-seo>',
      '<meta property="og:image:height" content="630" data-seo>',
      '<meta property="og:image:alt" content="Fund the tooling" data-seo>',
      '<meta name="twitter:card" content="summary_large_image" data-seo>',
      '<script type="application/ld+json" data-seo>',
    ];
    let at = -1;
    for (const tag of order) {
      const next = head.indexOf(tag);
      expect(next).toBeGreaterThan(at);
      at = next;
    }
  });

  it('marks every element with data-seo', () => {
    const head = renderHead(actionPage(), SITE, CARD);
    const tags = head.match(/<(title|meta|link|script)\b[^>]*>/g) ?? [];
    expect(tags.length).toBeGreaterThan(10);
    for (const tag of tags) expect(tag).toContain('data-seo');
  });

  it('uses the default card when the page has none', () => {
    const head = renderHead(actionPage({ card: null }), SITE, null);
    expect(head).toContain(
      `<meta property="og:image" content="${SITE}/images/og-default.png" data-seo>`,
    );
  });

  it('writes no canonical, og:url, og:image or JSON-LD without a site URL', () => {
    const head = renderHead(actionPage(), null, CARD);
    expect(head).not.toContain('rel="canonical"');
    expect(head).not.toContain('og:url');
    expect(head).not.toContain('og:image');
    expect(head).not.toContain('application/ld+json');
    expect(head).toContain('<title data-seo>');
  });

  it('marks a noindex page and leaves out its structured data', () => {
    const head = renderHead(actionPage({ noindex: true }), SITE, CARD);
    expect(head).toContain('<meta name="robots" content="noindex" data-seo>');
    expect(head).not.toContain('application/ld+json');
  });

  it('cannot be broken out of by hostile text', () => {
    const page = actionPage({
      title: EVIL,
      description: EVIL,
      summary: { heading: EVIL, lines: [EVIL] },
      breadcrumbs: [
        { name: 'Home', path: '/' },
        { name: EVIL, path: `/governance_actions/${ACTION}` },
      ],
      drepName: EVIL,
      kind: 'drep',
    });
    const head = renderHead(page, SITE, CARD);
    expect(head).not.toContain('<script>alert');
    expect(head).not.toContain('"></title>');
    const scripts = head.match(/<\/script>/g) ?? [];
    expect(scripts).toHaveLength(1);
    expect(JSON.stringify(jsonLdOf(head))).toContain('alert(1)');
  });

  it('describes home as a WebSite and an Organization', () => {
    const head = renderHead(
      actionPage({ kind: 'home', canonicalPath: '/', breadcrumbs: [] }),
      SITE,
      null,
    );
    const types = (jsonLdOf(head) as { '@graph': { '@type': string }[] })[
      '@graph'
    ].map((n) => n['@type']);
    expect(types).toEqual(['WebSite', 'Organization']);
  });

  it('gives a DRep a ProfilePage about a Person, with breadcrumbs', () => {
    const head = renderHead(
      actionPage({ kind: 'drep', drepName: 'YUTA' }),
      SITE,
      CARD,
    );
    const graph = (
      jsonLdOf(head) as {
        '@graph': { '@type': string; mainEntity?: { name: string } }[];
      }
    )['@graph'];
    expect(graph.map((n) => n['@type'])).toEqual([
      'BreadcrumbList',
      'ProfilePage',
    ]);
    expect(graph[1].mainEntity).toMatchObject({
      '@type': 'Person',
      name: 'YUTA',
    });
  });
});

describe('renderBody', () => {
  it('writes the heading and lines inside a marked main element', () => {
    const body = renderBody(actionPage());
    expect(body).toBe(
      '<main data-seo><h1>Fund the tooling</h1><p>A plan.</p></main>',
    );
  });

  it('escapes hostile text', () => {
    const body = renderBody(
      actionPage({ summary: { heading: EVIL, lines: [EVIL] } }),
    );
    expect(body).not.toContain('<script>');
  });

  it('has an empty default', () => {
    expect(DEFAULT_BODY).toBe('');
  });
});

describe('defaultHead', () => {
  it('is indexable site-wide defaults with no canonical', () => {
    const head = defaultHead(SITE);
    expect(head).toContain('<title data-seo>Cardano GovTool</title>');
    expect(head).not.toContain('noindex');
    expect(head).not.toContain('rel="canonical"');
    expect(head).toContain(`${SITE}/images/og-default.png`);
  });
});
