import { Test, TestingModule } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import * as request from 'supertest';
import { AppModule } from './../src/app.module';

describe('AppController (e2e)', () => {
  let app: INestApplication;

  beforeEach(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [AppModule],
    }).compile();

    app = moduleFixture.createNestApplication();
    await app.init();
  });

  it('/validate-metadata (POST)', () => {
    return request(app.getHttpServer())
      .get('/validate-metadata')
      .send({ hash: '', url: '' })
      .expect(200)
      .expect({ hash: '', url: '' });
  });
});
