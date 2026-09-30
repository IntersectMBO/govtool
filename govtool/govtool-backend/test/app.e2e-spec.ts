import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { App } from 'supertest/types';

import { AppController } from '../src/app.controller';
import { AppService } from '../src/app.service';
import { HealthController } from '../src/health/health.controller';

/**
 * Smoke test for the two routes that need no data layer. The data routes are
 * covered by `legacy-shape.spec.ts`, which asserts response bodies against a
 * stubbed provider; booting the whole `AppModule` here would require a live
 * db-sync, since `ProvidersModule` opens a pool on startup.
 */
describe('AppController (e2e)', () => {
  let app: INestApplication<App>;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      controllers: [AppController, HealthController],
      providers: [AppService],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('/ (GET)', () =>
    request(app.getHttpServer())
      .get('/')
      .expect(200)
      .expect('Govtool backend'));

  it('/health (GET)', () =>
    request(app.getHttpServer())
      .get('/health')
      .expect(200)
      .expect({ status: 'healthy' }));
});
