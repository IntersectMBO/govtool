import {
  Injectable,
  Logger,
  ServiceUnavailableException,
} from '@nestjs/common';

import { CacheService } from 'src/cache/cache.service';
import { ConfigService } from 'src/config/config.service';
import { DRepService } from 'src/drep/drep.service';
import { GovernanceActionsService } from 'src/governance-actions/governance-actions.service';

import { CardService, cardHash } from './card/card.service';
import { ForumClient } from './forum.client';
import {
  buildPage,
  isDRepIndexed,
  isLive,
  type CardInput,
  type PageData,
  type PageInfo,
} from './pages';
import { DEFAULT_BODY, defaultHead, renderBody, renderHead } from './render';
import {
  canonicalPathFor,
  isActionId,
  isDRepId,
  isForumId,
  resolvePath,
  type PageKind,
  type ResolvedPage,
  type UrlScheme,
} from './routes';
import { buildSitemap } from './sitemap';

const PAGE_TTL_SECONDS = 60;
const SITEMAP_TTL_SECONDS = 3_600;
const MAX_PATH_HEADER = 2_048;

export const CARD_KINDS = ['action', 'drep', 'forum'] as const;
type CardKind = (typeof CARD_KINDS)[number];

const PAGE_KIND: Record<CardKind, PageKind> = {
  action: 'action',
  drep: 'drep',
  forum: 'forumProposal',
};
const VALID_ID: Record<CardKind, (id: string) => boolean> = {
  action: isActionId,
  drep: isDRepId,
  forum: isForumId,
};

const isCardKind = (kind: string): kind is CardKind =>
  (CARD_KINDS as readonly string[]).includes(kind);

/** Where a card is served, through the frontend's /og/ location. */
export const cardPath = (card: CardInput | null): string | null =>
  card === null
    ? null
    : `/og/${card.kind}/${encodeURIComponent(card.id)}/${cardHash(card)}.png`;

export type CardResult = { png: Buffer } | { redirect: string };

/**
 * The SEO fragments, cards and sitemap. Every answer is built from cached
 * snapshots and stored documents; a failure gives the site-wide defaults,
 * never an error page, since these are included into every page load.
 */
@Injectable()
export class SeoService {
  private readonly logger = new Logger(SeoService.name);

  constructor(
    private readonly config: ConfigService,
    private readonly cache: CacheService,
    private readonly actions: GovernanceActionsService,
    private readonly dreps: DRepService,
    private readonly forum: ForumClient,
    private readonly cards: CardService,
  ) {}

  async head(rawPath: string | undefined): Promise<string> {
    const { siteUrl } = this.config.get();
    try {
      const page = await this.page(rawPath);
      return page === null
        ? defaultHead(siteUrl)
        : renderHead(page, siteUrl, cardPath(page.card));
    } catch (error) {
      this.warn('head', error);
      return defaultHead(siteUrl);
    }
  }

  async body(rawPath: string | undefined): Promise<string> {
    try {
      const page = await this.page(rawPath);
      return page === null ? DEFAULT_BODY : renderBody(page);
    } catch (error) {
      this.warn('body', error);
      return DEFAULT_BODY;
    }
  }

  /** The card at `hash`, a redirect to its current hash, or null (404). */
  async image(
    kind: string,
    id: string,
    hash: string,
  ): Promise<CardResult | null> {
    if (!isCardKind(kind) || !VALID_ID[kind](id)) return null;
    const scheme = this.config.get().seoUrlScheme;
    let card: CardInput | null;
    try {
      card = (await this.build(this.detail(kind, id, scheme), scheme)).card;
    } catch (error) {
      this.warn('image', error);
      throw new ServiceUnavailableException({
        errorType: 'ServiceUnavailableError',
        message: 'Preview cards are unavailable; try again shortly.',
      });
    }
    if (card === null) return null;
    const current = cardHash(card);
    if (hash !== current) return { redirect: cardPath(card) ?? '/' };
    return { png: await this.cards.render(card) };
  }

  /** The sitemap, or null when no site URL is configured. */
  sitemap(): Promise<string | null> {
    const { siteUrl, seoUrlScheme: scheme } = this.config.get();
    if (siteUrl === null) return Promise.resolve(null);
    return this.cache.getOrSet(
      'seo-sitemap',
      [scheme, siteUrl],
      async () =>
        buildSitemap(
          siteUrl,
          scheme,
          await this.actions.storedSeoActions(),
          await this.dreps.storedSeoDReps(),
        ),
      SITEMAP_TTL_SECONDS,
    );
  }

  /**
   * The page for a path, or null when there is no usable path (nginx sent
   * none), which gets indexable defaults rather than noindex. A recognised
   * page is cached a minute; forum proposals are not, since the forum
   * client keeps its own cache and a first request has only generic text.
   */
  private async page(rawPath: string | undefined): Promise<PageInfo | null> {
    if (
      rawPath === undefined ||
      rawPath === '' ||
      rawPath.length > MAX_PATH_HEADER
    ) {
      return null;
    }
    const scheme = this.config.get().seoUrlScheme;
    const resolved = resolvePath(rawPath, scheme);
    if (resolved.canonicalPath === null || resolved.kind === 'forumProposal') {
      return this.build(resolved, scheme);
    }
    return this.cache.getOrSet(
      'seo-page',
      [scheme, resolved.kind, resolved.canonicalPath],
      () => this.build(resolved, scheme),
      PAGE_TTL_SECONDS,
    );
  }

  private detail(kind: CardKind, id: string, scheme: UrlScheme): ResolvedPage {
    const pageKind = PAGE_KIND[kind];
    return {
      kind: pageKind,
      id,
      canonicalPath: canonicalPathFor(pageKind, id, scheme, false),
    };
  }

  private async build(
    resolved: ResolvedPage,
    scheme: UrlScheme,
  ): Promise<PageInfo> {
    const id = resolved.id ?? '';
    const data: PageData = {};
    switch (resolved.kind) {
      case 'action':
        data.action = await this.actions.storedSeoAction(id);
        break;
      case 'drep':
        data.drep = await this.dreps.storedSeoDRep(id);
        break;
      case 'forumProposal': {
        const forum = await this.forum.proposal(id);
        if (forum !== undefined) data.forum = forum;
        break;
      }
      case 'actionList':
        data.openActionCount = (await this.actions.storedSeoActions()).filter(
          isLive,
        ).length;
        break;
      case 'drepList':
        data.listedDRepCount = (await this.dreps.storedSeoDReps()).filter(
          isDRepIndexed,
        ).length;
        break;
      default:
        break;
    }
    return buildPage(resolved, data, scheme);
  }

  private warn(what: string, error: unknown): void {
    this.logger.warn(
      `SEO ${what} fell back to defaults: ${error instanceof Error ? error.message : String(error)}`,
    );
  }
}
