import { Injectable, Logger } from '@nestjs/common';

import { ConfigService } from 'src/config/config.service';

import type { SeoForumProposal } from './seo.types';

const TIMEOUT_MS = 400;
const MAX_BYTES = 256 * 1024;
const TTL_MS = 300_000;
const MAX_ENTRIES = 500;

type Entry = { value: SeoForumProposal | null; expiresAt: number };

const FAILED = Symbol('forum fetch failed');

const isObject = (v: unknown): v is Record<string, unknown> =>
  typeof v === 'object' && v !== null && !Array.isArray(v);

const str = (v: unknown): string | null =>
  typeof v === 'string' && v.trim() !== '' ? v : null;

/**
 * Forum proposals for the SEO pages. A page load never waits on the forum: a
 * proposal not cached yet answers undefined (not known yet) at once and is
 * fetched in the background, so the next request has it; null means the forum
 * has no such proposal. Answers, a miss included, are kept
 * for five minutes in a bounded map (CacheService holds promises and has no
 * read that does not fetch).
 */
@Injectable()
export class ForumClient {
  private readonly logger = new Logger(ForumClient.name);
  private readonly entries = new Map<string, Entry>();
  private readonly inFlight = new Map<string, Promise<void>>();

  constructor(private readonly config: ConfigService) {}

  proposal(id: string): Promise<SeoForumProposal | null | undefined> {
    const base = this.config.get().pdfApiUrl;
    if (base === null) return Promise.resolve(null);

    const entry = this.entries.get(id);
    if (entry !== undefined && entry.expiresAt > Date.now()) {
      return Promise.resolve(entry.value);
    }
    if (!this.inFlight.has(id)) {
      const fetching = this.fetch(base, id)
        .then((answer) => {
          // A failed fetch keeps what was known (a stale proposal, or
          // nothing): only the forum saying "not found" makes it null.
          if (answer !== FAILED) this.remember(id, answer);
        })
        .finally(() => this.inFlight.delete(id));
      this.inFlight.set(id, fetching);
    }
    // A stale answer is still better than none while the refresh runs.
    return Promise.resolve(entry === undefined ? undefined : entry.value);
  }

  /** Resolves when every background fetch under way has finished. */
  async settled(): Promise<void> {
    await Promise.all([...this.inFlight.values()]);
  }

  private remember(id: string, value: SeoForumProposal | null): void {
    this.entries.delete(id);
    this.entries.set(id, { value, expiresAt: Date.now() + TTL_MS });
    if (this.entries.size > MAX_ENTRIES) {
      const [oldest] = this.entries.keys();
      this.entries.delete(oldest);
    }
  }

  /**
   * The proposal; null when the forum says it has none (400 "Proposal not
   * found", 404, or no published content); FAILED for anything else, such as
   * a timeout, a 5xx or a body that is not the expected JSON.
   */
  private async fetch(
    base: string,
    id: string,
  ): Promise<SeoForumProposal | null | typeof FAILED> {
    try {
      const response = await fetch(`${base}/proposals/${id}`, {
        headers: {
          Accept: 'application/json',
          'User-Agent': 'GovTool/Backend',
        },
        redirect: 'error',
        signal: AbortSignal.timeout(TIMEOUT_MS),
      });
      if (response.status === 400 || response.status === 404) return null;
      if (!response.ok) {
        this.logger.warn(
          `Could not read forum proposal ${id}: the forum answered ${response.status}`,
        );
        return FAILED;
      }
      const text = await response.text();
      if (Buffer.byteLength(text) > MAX_BYTES) return FAILED;
      return toProposal(id, JSON.parse(text) as unknown);
    } catch (error) {
      this.logger.warn(
        `Could not read forum proposal ${id}: ${error instanceof Error ? error.message : String(error)}`,
      );
      return FAILED;
    }
  }
}

/** The Strapi-shaped single proposal, its active content's name and abstract. */
function toProposal(id: string, body: unknown): SeoForumProposal | null {
  if (!isObject(body) || !isObject(body['data'])) return null;
  const attributes = body['data']['attributes'];
  if (!isObject(attributes)) return null;
  const content = attributes['content'];
  if (!isObject(content) || !isObject(content['attributes'])) return null;
  const title = str(content['attributes']['prop_name']);
  if (title === null) return null;
  return {
    id,
    title,
    abstract: str(content['attributes']['prop_abstract']),
    updatedAt: str(attributes['updatedAt']),
  };
}
