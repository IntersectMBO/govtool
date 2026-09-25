import { Test } from '@nestjs/testing';
import { INestApplication } from '@nestjs/common';
import * as express from 'express';
import request from 'supertest';
import { ProposalController } from '../src/proposal/proposal.controller';
import { ProposalService } from '../src/proposal/proposal.service';
import { DRepController } from '../src/drep/drep.controller';
import { DRepService } from '../src/drep/drep.service';
import { IpfsController } from '../src/ipfs/ipfs.controller';
import { IpfsService } from '../src/ipfs/ipfs.service';
import type { GovernanceActionType } from '../src/proposal/proposal.type';
import type { Server } from 'node:http';

describe('HTTP input contracts', () => {
  let app: INestApplication<Server>;
  const proposals = {
    list: jest.fn((params: Parameters<ProposalService['list']>[0]) =>
      Promise.resolve(params),
    ),
    getEnactedDetails: jest.fn(() => Promise.resolve(null)),
  };
  const dreps = {
    list: jest.fn((params: Parameters<DRepService['list']>[0]) =>
      Promise.resolve(params),
    ),
    getVotes: jest.fn((_id: string, types: GovernanceActionType[]) =>
      Promise.resolve(types),
    ),
  };
  const ipfs = {
    upload: jest.fn(() => Promise.resolve({ ipfsCid: 'test' })),
  };
  beforeAll(async () => {
    const module = await Test.createTestingModule({
      controllers: [ProposalController, DRepController, IpfsController],
      providers: [
        { provide: ProposalService, useValue: proposals },
        { provide: DRepService, useValue: dreps },
        { provide: IpfsService, useValue: ipfs },
      ],
    }).compile();
    app = module.createNestApplication<INestApplication<Server>>();
    app.use(express.text({ type: 'text/plain' }));
    await app.init();
  });
  afterAll(async () => {
    await app.close();
  });
  it.each(['/proposal/list', '/drep/list'])(
    'validates pagination on %s',
    async (route) => {
      for (const value of [
        'abc',
        '-1',
        '1.5',
        'Infinity',
        '1e2',
        '9007199254740993',
        '',
      ]) {
        await request(app.getHttpServer())
          .get(`${route}?page=${value}`)
          .expect(400);
        await request(app.getHttpServer())
          .get(`${route}?pageSize=${value}`)
          .expect(400);
      }
      await request(app.getHttpServer())
        .get(`${route}?page=0&pageSize=0`)
        .expect(200)
        .expect((response) => {
          const body = response.body as { page: number; pageSize: number };
          expect(body).toMatchObject({ page: 0, pageSize: 0 });
        });
      await request(app.getHttpServer())
        .get(route)
        .expect(200)
        .expect((response) => {
          const body = response.body as { page: number; pageSize: number };
          expect(body).toMatchObject({ page: 0, pageSize: 10 });
        });
    },
  );
  it('accepts both type array encodings on proposal and vote routes', async () => {
    await request(app.getHttpServer())
      .get('/proposal/list?type=InfoAction&type[]=NoConfidence')
      .expect(200)
      .expect((response) => {
        const body = response.body as { type: GovernanceActionType[] };
        expect(body.type).toEqual(['InfoAction', 'NoConfidence']);
      });
    await request(app.getHttpServer())
      .get('/drep/getVotes/ab?type[]=InfoAction&type[]=NoConfidence')
      .expect(200)
      .expect(['InfoAction', 'NoConfidence']);
  });
  it.each([
    '/proposal/list?type=invalid',
    '/proposal/list?type[]=invalid',
    '/proposal/list?type=InfoAction&type[]=invalid',
    '/proposal/list?sort=invalid',
    '/proposal/list?sort=NewestCreated&sort=MostYesVotes',
    '/proposal/enacted-details?type=invalid',
    '/proposal/enacted-details?type=',
    '/drep/getVotes/ab?type=invalid',
    '/drep/getVotes/ab?type[]=invalid',
    '/drep/getVotes/ab?sort=invalid',
    '/drep/list?status=invalid',
    '/drep/list?status[]=invalid',
    '/drep/list?sort=invalid',
    '/drep/list?status=',
  ])('rejects invalid enum request %s', async (url) => {
    await request(app.getHttpServer()).get(url).expect(400);
  });
  it('accepts valid enum values and omitted optional values', async () => {
    await request(app.getHttpServer())
      .get('/proposal/list?type=InfoAction&sort=MostYesVotes')
      .expect(200);
    await request(app.getHttpServer())
      .get('/drep/list?status=Active&status[]=Retired&sort=VotingPower')
      .expect(200);
    await request(app.getHttpServer())
      .get('/proposal/enacted-details?type=InfoAction')
      .expect(200);
    expect(proposals.getEnactedDetails).toHaveBeenLastCalledWith('InfoAction');
    await request(app.getHttpServer())
      .get('/proposal/enacted-details')
      .expect(200);
    expect(proposals.getEnactedDetails).toHaveBeenLastCalledWith(undefined);
  });
  it('returns 415 for unsupported upload types and accepts plain text', async () => {
    await request(app.getHttpServer())
      .post('/ipfs/upload')
      .send({ body: 'data' })
      .expect(415);
    await request(app.getHttpServer())
      .post('/ipfs/upload')
      .type('application/octet-stream')
      .send('data')
      .expect(415);
    expect(ipfs.upload).not.toHaveBeenCalled();
    await request(app.getHttpServer())
      .post('/ipfs/upload')
      .type('text/plain')
      .send('data')
      .expect(201);
    expect(ipfs.upload).toHaveBeenCalledWith(
      undefined,
      'data',
      expect.any(String),
    );
  });
});
