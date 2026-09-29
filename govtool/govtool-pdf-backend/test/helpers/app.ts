// Boot the real AppModule through the production pipeline (configureApp).

import type { INestApplication } from '@nestjs/common';
import { Test } from '@nestjs/testing';
import type { Server } from 'node:http';
import type { AddressInfo } from 'node:net';
import request from 'supertest';
import { AppModule } from '../../src/app.module';
import { configureApp } from '../../src/app.setup';
import { loadConfig } from '../../src/config/config';
import { PrismaService } from '../../src/prisma/prisma.service';

export interface TestApp {
  app: INestApplication;
  prisma: PrismaService;
  /** A fresh supertest request against the app. Query strings are sent as written. */
  api: () => ReturnType<typeof request>;
  close: () => Promise<void>;
}

/**
 * `env` overrides (e.g. `{ CHALLENGE_TTL_SECONDS: '1' }`) apply while this
 * app lives and are restored on close.
 */
export async function createTestApp(env: Record<string, string | undefined> = {}): Promise<TestApp> {
  const saved: Record<string, string | undefined> = {};
  for (const [k, v] of Object.entries(env)) {
    saved[k] = process.env[k];
    if (v === undefined) delete process.env[k];
    else process.env[k] = v;
  }
  const config = loadConfig();
  const moduleRef = await Test.createTestingModule({ imports: [AppModule] }).compile();
  const app = moduleRef.createNestApplication({ bodyParser: false, logger: ['error', 'warn'] });
  configureApp(app, config);
  // Listen once: supertest's per-request listen/close on a shared server
  // races when a test awaits another request while building one.
  await app.listen(0, '127.0.0.1');
  const { port } = (app.getHttpServer() as Server).address() as AddressInfo;
  const base = `http://127.0.0.1:${port}`;
  return {
    app,
    prisma: app.get(PrismaService),
    api: () => request(base),
    close: async () => {
      await app.close();
      for (const [k, v] of Object.entries(saved)) {
        if (v === undefined) delete process.env[k];
        else process.env[k] = v;
      }
    },
  };
}
