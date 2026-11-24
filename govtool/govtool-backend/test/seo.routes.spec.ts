import type { INestApplication } from '@nestjs/common';
import { Test } from '@nestjs/testing';
import request from 'supertest';
import type { App } from 'supertest/types';

import { CacheService } from '../src/cache/cache.service';
import { ConfigService } from '../src/config/config.service';
import { DRepService } from '../src/drep/drep.service';
import { GovernanceActionsService } from '../src/governance-actions/governance-actions.service';
import { CardService, cardHash } from '../src/seo/card/card.service';
import { ForumClient } from '../src/seo/forum.client';
import { SeoController } from '../src/seo/seo.controller';
import { SeoService } from '../src/seo/seo.service';
import type { SeoAction, SeoDRep } from '../src/seo/seo.types';
import { actionId, drepId } from './ids';

jest.setTimeout(60_000);

const ACTION = actionId('ab'.repeat(32), 0);
const DREP = drepId('a');

const action: SeoAction = {
  cip129Id: ACTION,
  type: 'InfoAction',
  title: 'Fund the tooling',
  abstract: 'A plan.',
  expiryDate: '2026-11-30T21:44:51Z',
  expiryEpoch: 671,
  createdDate: '2026-10-28T10:27:09.000Z',
  status: {
    ratified_epoch: null,
    enacted_epoch: null,
    dropped_epoch: null,
    expired_epoch: null,
  },
  statusTimes: {
    ratified_time: null,
    enacted_time: null,
    dropped_time: null,
    expired_time: null,
  },
};

const drep: SeoDRep = {
  view: DREP,
  givenName: 'YUTA',
  objectives: null,
  status: 'Active',
  votingPower: 1_000_000n,
  doNotList: false,
  metadataError: null,
  hasMetadata: true,
  latestRegistrationDate: '2025-02-10T15:55:32.000Z',
};

const CARD = {
  kind: 'action' as const,
  id: ACTION,
  title: 'Fund the tooling',
  label: 'Info action · Voting open',
};

async function app(options: {
  siteUrl?: string | null;
  failing?: boolean;
}): Promise<INestApplication<App>> {
  const actions: Partial<GovernanceActionsService> = {
    storedSeoAction: jest.fn((id: string) =>
      options.failing
        ? Promise.reject(new Error('provider down'))
        : Promise.resolve(id === ACTION ? action : null),
    ),
    storedSeoActions: jest.fn(() => Promise.resolve([action])),
  };
  const dreps: Partial<DRepService> = {
    storedSeoDRep: jest.fn(() => Promise.resolve(drep)),
    storedSeoDReps: jest.fn(() => Promise.resolve([drep])),
  };
  const forum: Partial<ForumClient> = {
    proposal: jest.fn(() => Promise.resolve(undefined)),
  };
  const config = {
    get: () => ({
      siteUrl:
        options.siteUrl === undefined ? 'https://gov.tools' : options.siteUrl,
      seoUrlScheme: 'develop',
      cacheDurationSeconds: 0,
      drepListCacheDurationSeconds: 0,
      cacheMaxEntries: 1_000,
    }),
  } as unknown as ConfigService;

  const module = await Test.createTestingModule({
    controllers: [SeoController],
    providers: [
      SeoService,
      CardService,
      { provide: ConfigService, useValue: config },
      { provide: CacheService, useValue: new CacheService(config) },
      { provide: GovernanceActionsService, useValue: actions },
      { provide: DRepService, useValue: dreps },
      { provide: ForumClient, useValue: forum },
    ],
  }).compile();
  const nest = module.createNestApplication<INestApplication<App>>();
  await nest.init();
  return nest;
}

describe('SEO routes', () => {
  let server: INestApplication<App>;

  beforeAll(async () => {
    server = await app({});
  });

  afterAll(async () => server.close());

  it('writes an action page head from the X-Seo-Path header', async () => {
    const res = await request(server.getHttpServer())
      .get('/seo/head')
      .set('X-Seo-Path', `/governance_actions/${ACTION}?utm_source=x`)
      .expect(200)
      .expect('Content-Type', /text\/html/);
    expect(res.text).toContain(
      '<title data-seo>Fund the tooling | Cardano GovTool</title>',
    );
    expect(res.text).toContain(
      `https://gov.tools/og/action/${ACTION}/${cardHash(CARD)}.png`,
    );
  });

  it('writes the body summary', async () => {
    const res = await request(server.getHttpServer())
      .get('/seo/body')
      .set('X-Seo-Path', `/governance_actions/${ACTION}`)
      .expect(200);
    expect(res.text).toBe(
      '<main data-seo><h1>Fund the tooling</h1><p>Info action · voting open until 30 Nov 2026. A plan.</p></main>',
    );
  });

  it.each([
    ['no header', undefined],
    ['an over-long header', `/${'a'.repeat(3000)}`],
  ])('answers indexable defaults for %s', async (_, path) => {
    const call = request(server.getHttpServer()).get('/seo/head');
    const res = await (
      path === undefined ? call : call.set('X-Seo-Path', path)
    ).expect(200);
    expect(res.text).toContain('<title data-seo>Cardano GovTool</title>');
    expect(res.text).not.toContain('noindex');
  });

  it('marks an unknown path noindex', async () => {
    const res = await request(server.getHttpServer())
      .get('/seo/head')
      .set('X-Seo-Path', '/nothing/here')
      .expect(200);
    expect(res.text).toContain('noindex');
  });

  it('serves the current card as an immutable PNG', async () => {
    const res = await request(server.getHttpServer())
      .get(`/seo/image/action/${ACTION}/${cardHash(CARD)}.png`)
      .expect(200)
      .expect('Content-Type', 'image/png')
      .expect('Cache-Control', 'public, max-age=31536000, immutable');
    expect((res.body as Buffer).subarray(1, 4).toString()).toBe('PNG');
  });

  it('redirects a stale card hash to the current one', async () => {
    await request(server.getHttpServer())
      .get(`/seo/image/action/${ACTION}/000000000000.png`)
      .expect(301)
      .expect('Location', `/og/action/${ACTION}/${cardHash(CARD)}.png`)
      .expect('Cache-Control', 'no-store');
  });

  it.each([
    `/seo/image/action/${actionId('cd'.repeat(32), 0)}/000000000000.png`,
    `/seo/image/poll/${ACTION}/000000000000.png`,
    `/seo/image/action/${ACTION}/nothex.png`,
    '/seo/image/action/zz/000000000000.png',
  ])('answers 404 for %s', async (path) => {
    await request(server.getHttpServer()).get(path).expect(404);
  });

  it.each([
    ['X-Forwarded-For', '203.0.113.7'],
    ['X-Real-IP', '203.0.113.7'],
    ['Forwarded', 'for=203.0.113.7'],
    ['X-Forwarded-Host', 'gov.tools'],
  ])('refuses a call that came through a proxy (%s)', async (header, value) => {
    const server_ = server.getHttpServer();
    await request(server_).get('/seo/head').set(header, value).expect(404);
    await request(server_)
      .get('/seo/sitemap.xml')
      .set(header, value)
      .expect(404);
    await request(server_)
      .get(`/seo/image/action/${ACTION}/${cardHash(CARD)}.png`)
      .set(header, value)
      .expect(404);
  });

  it('serves the sitemap as XML', async () => {
    const res = await request(server.getHttpServer())
      .get('/seo/sitemap.xml')
      .expect(200)
      .expect('Content-Type', /application\/xml/);
    expect(res.text).toContain(
      `https://gov.tools/governance_actions/${ACTION}`,
    );
    expect(res.text).toContain(`https://gov.tools/drep_directory/${DREP}`);
  });
});

describe('SEO routes without a site URL or with a failing provider', () => {
  it('answers 404 for the sitemap without a site URL', async () => {
    const nest = await app({ siteUrl: null });
    await request(nest.getHttpServer()).get('/seo/sitemap.xml').expect(404);
    await nest.close();
  });

  it('answers 200 with defaults when the provider fails', async () => {
    const nest = await app({ failing: true });
    const res = await request(nest.getHttpServer())
      .get('/seo/head')
      .set('X-Seo-Path', `/governance_actions/${ACTION}`)
      .expect(200);
    expect(res.text).toContain('<title data-seo>Cardano GovTool</title>');
    expect(res.text).not.toContain('noindex');
    await nest.close();
  });
});
