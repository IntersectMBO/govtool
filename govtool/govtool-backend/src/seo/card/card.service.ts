import { createHash } from 'node:crypto';

import {
  Inject,
  Injectable,
  Optional,
  ServiceUnavailableException,
} from '@nestjs/common';
import { renderAsync } from '@resvg/resvg-js';
import satori, { type SatoriOptions } from 'satori';

import type { CardInput } from '../pages';
import { plainText, stripEmoji, truncate } from '../text';
import { assetDataUri, cardFonts, coveredCodePoints } from './fonts';
import {
  CARD_HEIGHT,
  CARD_WIDTH,
  cardElement,
  type CardImages,
  type CardText,
} from './template';

/** Bump when the template changes, so every card gets a new URL. */
export const CARD_TEMPLATE_VERSION = 1;
const CARD_TITLE_MAX = 140;

export const CARD_LIMITS = Symbol('CARD_LIMITS');
export type CardLimits = {
  maxConcurrent: number;
  waitMs: number;
  cacheSize: number;
};
const DEFAULT_LIMITS: CardLimits = {
  maxConcurrent: 2,
  waitMs: 5_000,
  cacheSize: 200,
};

const DEFAULT_TEXT: CardText = {
  title: 'Cardano GovTool',
  label: 'Vote, delegate and follow Cardano governance',
};
const DEFAULT_KEY = 'default';

/** The text as drawn: plain, without emoji, one line, capped. */
const drawable = (text: string, max: number): string =>
  truncate(stripEmoji(plainText(text)).replace(/\s+/g, ' ').trim(), max);

const cardText = (card: CardInput): CardText => ({
  title: drawable(card.title, CARD_TITLE_MAX),
  label: drawable(card.label, CARD_TITLE_MAX),
});

/** SHA-256 over everything drawn on the card, as 64 hex. */
export const cardDigest = (card: CardInput): string => {
  const { title, label } = cardText(card);
  return createHash('sha256')
    .update(JSON.stringify([CARD_TEMPLATE_VERSION, card.kind, title, label]))
    .digest('hex');
};

/**
 * The 12 hex in a card's URL. Only the URL is this short: the render cache is
 * keyed by the full digest, so a card whose text collides with another's in
 * 48 bits can never be served under the other's URL.
 */
export const cardHash = (card: CardInput): string =>
  cardDigest(card).slice(0, 12);

/**
 * Whether the bundled fonts draw every character of the text, by their own
 * character maps: Latin, Russian Cyrillic, unaccented Greek and Japanese
 * pass, while accented Greek, Ukrainian, Korean, Arabic and the Simplified
 * Chinese characters outside the Japanese set do not. Anything not drawn gets
 * the default card, never boxes.
 */
export const scriptSupported = (text: string): boolean => {
  const covered = coveredCodePoints();
  for (const char of text) {
    if (!/\s/.test(char) && !covered.has(char.codePointAt(0) ?? -1)) {
      return false;
    }
  }
  return true;
};

/**
 * Renders preview cards with Satori (layout to SVG) and resvg (SVG to PNG).
 * Renders are deduplicated by key and kept in an LRU, and at most
 * `maxConcurrent` run at once; a request that waits longer than `waitMs` for
 * a slot answers 503 rather than queueing without bound.
 */
@Injectable()
export class CardService {
  private readonly limits: CardLimits;
  private readonly cards = new Map<string, Promise<Buffer>>();
  private active = 0;
  private readonly waiting: (() => void)[] = [];
  private images: CardImages | undefined;

  constructor(@Optional() @Inject(CARD_LIMITS) limits?: Partial<CardLimits>) {
    this.limits = { ...DEFAULT_LIMITS, ...limits };
  }

  render(card: CardInput): Promise<Buffer> {
    const text = cardText(card);
    if (!scriptSupported(`${text.title} ${text.label}`)) {
      return this.renderDefault();
    }
    return this.cached(cardDigest(card), () => this.renderPng(text));
  }

  renderDefault(): Promise<Buffer> {
    return this.cached(DEFAULT_KEY, () => this.renderPng(DEFAULT_TEXT));
  }

  /** The Satori step alone; public so tests can see what is drawn. */
  renderSvg(text: CardText | CardInput): Promise<string> {
    const drawn = 'kind' in text ? cardText(text) : text;
    return satori(cardElement(drawn, this.cardImages()), this.satoriOptions());
  }

  /**
   * Every font up front and no loadAdditionalAsset: Satori appends fonts a
   * hook returns to its cache on each render, which grows without bound.
   */
  satoriOptions(): SatoriOptions {
    return { width: CARD_WIDTH, height: CARD_HEIGHT, fonts: cardFonts() };
  }

  private cardImages(): CardImages {
    return (this.images ??= {
      logo: assetDataUri('logo.svg', 'image/svg+xml'),
      sphere: assetDataUri('sphere.png', 'image/png'),
    });
  }

  private cached(key: string, make: () => Promise<Buffer>): Promise<Buffer> {
    const existing = this.cards.get(key);
    if (existing !== undefined) {
      this.cards.delete(key);
      this.cards.set(key, existing);
      return existing;
    }
    const made = make();
    this.cards.set(key, made);
    made.catch(() => {
      if (this.cards.get(key) === made) this.cards.delete(key);
    });
    if (this.cards.size > this.limits.cacheSize) {
      const [oldest] = this.cards.keys();
      this.cards.delete(oldest);
    }
    return made;
  }

  private async renderPng(text: CardText): Promise<Buffer> {
    await this.acquire();
    try {
      const svg = await this.renderSvg(text);
      // Rasterised on libuv's thread pool, not the event loop that every
      // page load's head and body requests share.
      const rendered = await renderAsync(svg, {
        fitTo: { mode: 'width', value: CARD_WIDTH },
      });
      return Buffer.from(rendered.asPng());
    } finally {
      this.release();
    }
  }

  private acquire(): Promise<void> {
    if (this.active < this.limits.maxConcurrent) {
      this.active += 1;
      return Promise.resolve();
    }
    return new Promise((resolve, reject) => {
      const grant = () => {
        clearTimeout(timer);
        this.active += 1;
        resolve();
      };
      const timer = setTimeout(() => {
        const at = this.waiting.indexOf(grant);
        if (at !== -1) this.waiting.splice(at, 1);
        reject(
          new ServiceUnavailableException({
            errorType: 'ServiceUnavailableError',
            message: 'Preview cards are busy; try again shortly.',
          }),
        );
      }, this.limits.waitMs);
      this.waiting.push(grant);
    });
  }

  private release(): void {
    this.active -= 1;
    this.waiting.shift()?.();
  }
}
