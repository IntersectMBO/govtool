import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import request from 'supertest';
import { App } from 'supertest/types';
import { DbService } from '../src/db/db.service';
import { ConfigService } from '../src/config/config.service';
import { AppModule } from './../src/app.module';

describe('AppController (e2e)', () => {
  let app: INestApplication<App>;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    })
      .overrideProvider(ConfigService)
      .useValue({
        get: () => ({
          cacheDurationSeconds: 20,
          drepListCacheDurationSeconds: 600,
          cacheMaxEntries: 256,
        }),
      })
      .overrideProvider(DbService)
      .useValue({
        query: jest.fn().mockRejectedValue(new Error('DB unavailable')),
      })
      .compile();

    app = moduleFixture.createNestApplication();
    app.useLogger(false);
    await app.init();
  });

  afterEach(async () => {
    await app.close();
  });

  it('/health remains available with the DB down', () => {
    return request(app.getHttpServer())
      .get('/health')
      .expect(200)
      .expect({ status: 'healthy' });
  });

  it('/ (GET)', () => {
    return request(app.getHttpServer())
      .get('/')
      .expect(200)
      .expect('Govtool backend');
  });
});
