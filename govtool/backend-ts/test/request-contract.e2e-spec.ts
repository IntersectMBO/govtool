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

describe('HTTP input contracts', () => {
  let app: INestApplication;
  const proposals = { list: jest.fn(async (params) => params) };
  const dreps = {
    list: jest.fn(async (params) => params),
    getVotes: jest.fn(async (_id, types) => types),
  };
  const ipfs = { upload: jest.fn(async () => ({ ipfsCid: 'test' })) };
  beforeAll(async () => {
    const module = await Test.createTestingModule({
      controllers: [ProposalController, DRepController, IpfsController],
      providers: [
        { provide: ProposalService, useValue: proposals },
        { provide: DRepService, useValue: dreps },
        { provide: IpfsService, useValue: ipfs },
      ],
    }).compile();
    app = module.createNestApplication();
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
        .expect(({ body }) => {
          expect(body).toMatchObject({ page: 0, pageSize: 0 });
        });
      await request(app.getHttpServer())
        .get(route)
        .expect(200)
        .expect(({ body }) => {
          expect(body).toMatchObject({ page: 0, pageSize: 10 });
        });
    },
  );
  it('accepts both type array encodings on proposal and vote routes', async () => {
    await request(app.getHttpServer())
      .get('/proposal/list?type=InfoAction&type[]=NoConfidence')
      .expect(200)
      .expect(({ body }) => {
        expect(body.type).toEqual(['InfoAction', 'NoConfidence']);
      });
    await request(app.getHttpServer())
      .get('/drep/getVotes/ab?type[]=InfoAction&type[]=NoConfidence')
      .expect(200)
      .expect(['InfoAction', 'NoConfidence']);
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
    expect(ipfs.upload).toHaveBeenCalledWith('data.txt', 'data');
  });
});
