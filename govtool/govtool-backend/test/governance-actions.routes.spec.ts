import type { INestApplication } from '@nestjs/common';
import { Test } from '@nestjs/testing';
import request from 'supertest';
import type { App } from 'supertest/types';

import {
  GovernanceActionsController,
  GovernanceMiscController,
} from '../src/governance-actions/governance-actions.controller';
import { GovernanceActionsService } from '../src/governance-actions/governance-actions.service';

describe('GovTool governance action HTTP routes', () => {
  let app: INestApplication<App>;
  const records: Partial<GovernanceActionsService> = {
    list: jest.fn(() => Promise.resolve([])),
    getMetadata: jest.fn(() =>
      Promise.resolve({ metadataStatus: null, metadataValid: true }),
    ),
  };

  beforeAll(async () => {
    const module = await Test.createTestingModule({
      controllers: [GovernanceActionsController, GovernanceMiscController],
      providers: [{ provide: GovernanceActionsService, useValue: records }],
    }).compile();
    app = module.createNestApplication();
    await app.init();
  });

  afterAll(async () => app.close());

  it('serves records at the backend root with the existing list defaults', async () => {
    await request(app.getHttpServer())
      .get('/governance-actions')
      .expect(200)
      .expect([]);
    expect(records.list).toHaveBeenCalledWith({
      search: '',
      filters: [],
      sort: 'newestFirst',
      page: 1,
      limit: 12,
    });
  });

  it('keeps metadata beneath the governance actions route', async () => {
    await request(app.getHttpServer())
      .get('/governance-actions/metadata')
      .query({ url: 'https://example.com/action.json', hash: 'abcd' })
      .expect(200)
      .expect({ metadataStatus: null, metadataValid: true });
    expect(records.getMetadata).toHaveBeenCalledWith(
      'https://example.com/action.json',
      'abcd',
    );
  });

  it('rejects invalid lifecycle filters', async () => {
    await request(app.getHttpServer())
      .get('/governance-actions?filters=unknown')
      .expect(400);
  });

  it('rejects invalid historical epoch parameters on the supporting route', async () => {
    await request(app.getHttpServer())
      .get('/misc/epoch/params?epoch=-1')
      .expect(400);
  });
});
