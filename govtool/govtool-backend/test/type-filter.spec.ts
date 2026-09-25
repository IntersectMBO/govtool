/**
 * The frontend filters by governance action type with a bracketed, percent-
 * encoded parameter (axios sends `type%5B%5D=ParameterChange`), which Express
 * keys as `type[]`. These go through real HTTP so the query parsing is covered,
 * not just the controller method.
 */
import { INestApplication } from '@nestjs/common';
import { Test } from '@nestjs/testing';
import request from 'supertest';
import { App } from 'supertest/types';

import { DRepController } from '../src/drep/drep.controller';
import { DRepService } from '../src/drep/drep.service';
import { ProposalController } from '../src/proposal/proposal.controller';
import { ProposalService } from '../src/proposal/proposal.service';

const DREP_ID = 'drep1' + 'q'.repeat(52);

async function app(
  proposals: Partial<ProposalService>,
  dreps: Partial<DRepService>,
): Promise<INestApplication<App>> {
  const moduleRef = await Test.createTestingModule({
    controllers: [ProposalController, DRepController],
    providers: [
      { provide: ProposalService, useValue: proposals },
      { provide: DRepService, useValue: dreps },
    ],
  }).compile();
  const nest = moduleRef.createNestApplication<INestApplication<App>>({
    logger: false,
  });
  await nest.init();
  return nest;
}

describe('type[] query parameter', () => {
  let nest: INestApplication<App>;
  const list = jest.fn().mockResolvedValue({ elements: [] });
  const getVotes = jest.fn().mockResolvedValue([]);

  beforeAll(async () => {
    nest = await app({ list }, { getVotes });
  });
  afterAll(() => nest.close());
  beforeEach(() => jest.clearAllMocks());

  it('filters /proposal/list by the bracketed form the frontend sends', async () => {
    await request(nest.getHttpServer())
      .get(
        '/proposal/list?page=0&pageSize=10&type%5B%5D=ParameterChange&type%5B%5D=InfoAction',
      )
      .expect(200);
    expect(list).toHaveBeenCalledWith(
      expect.objectContaining({ type: ['ParameterChange', 'InfoAction'] }),
    );
  });

  it('still accepts the plain form on /proposal/list', async () => {
    await request(nest.getHttpServer())
      .get('/proposal/list?type=TreasuryWithdrawals')
      .expect(200);
    expect(list).toHaveBeenCalledWith(
      expect.objectContaining({ type: ['TreasuryWithdrawals'] }),
    );
  });

  it('rejects an unknown bracketed type', async () => {
    await request(nest.getHttpServer())
      .get('/proposal/list?type%5B%5D=NotAType')
      .expect(400);
    expect(list).not.toHaveBeenCalled();
  });

  it('filters /drep/getVotes by the bracketed form', async () => {
    await request(nest.getHttpServer())
      .get(`/drep/getVotes/${DREP_ID}?type%5B%5D=HardForkInitiation`)
      .expect(200);
    expect(getVotes).toHaveBeenCalledWith(
      DREP_ID,
      ['HardForkInitiation'],
      undefined,
      undefined,
    );
  });
});
