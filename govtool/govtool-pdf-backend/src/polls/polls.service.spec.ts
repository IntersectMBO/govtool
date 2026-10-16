import { ApiError } from '../common/errors';
import type { PrismaService } from '../prisma/prisma.service';
import { caller, prismaStub } from '../proposals/__fixtures__/prisma-stub';
import { PollsService } from './polls.service';

const T0 = new Date('2026-09-26T10:00:00.000Z');

async function error(p: Promise<unknown>): Promise<[string, unknown]> {
  try {
    await p;
  } catch (e) {
    if (e instanceof ApiError) return [e.errorName, e.message === 'Bad Request' ? e.details : e.message];
    throw e;
  }
  throw new Error('expected an ApiError');
}

describe('PollsService', () => {
  const setup = () => {
    const prisma = prismaStub(['poll', 'pollVote', 'proposal']);
    return { prisma, service: new PollsService(prisma as unknown as PrismaService) };
  };

  it('create forces every poll field (Δ30)', async () => {
    const { prisma, service } = setup();
    prisma.proposal.findUnique.mockResolvedValue({ id: 3, userId: 1 });
    prisma.poll.count.mockResolvedValue(0);
    prisma.poll.create.mockImplementation(({ data }: { data: Record<string, unknown> }) =>
      Promise.resolve({ id: 1, createdAt: T0, updatedAt: T0, ...data }),
    );
    await service.create(
      { proposal_id: '3', poll_yes: 9, poll_no: 9, is_poll_active: false, poll_start_dt: '2000-01-01' },
      caller(1),
    );
    const data = (prisma.poll.create.mock.calls[0] as [{ data: Record<string, unknown> }])[0].data;
    expect(data).toMatchObject({ proposalId: 3, isActive: true, yes: 0, no: 0 });
    expect((data.startDt as Date).getFullYear()).toBeGreaterThan(2000);
  });

  it('create: not the owner is F, an active poll is BD', async () => {
    const { prisma, service } = setup();
    prisma.proposal.findUnique.mockResolvedValue({ id: 3, userId: 1 });
    expect(await error(service.create({ proposal_id: 3 }, caller(2)))).toEqual([
      'ForbiddenError',
      'User is not owner of this proposal',
    ]);
    prisma.poll.count.mockResolvedValue(1);
    expect(await error(service.create({ proposal_id: 3 }, caller(1)))).toEqual([
      'BadRequestError',
      'There is already an active pool for this proposal',
    ]);
    const unique = Object.assign(new Error('dup'), { code: 'P2002' });
    prisma.poll.count.mockResolvedValue(0);
    prisma.poll.create.mockRejectedValue(unique);
    expect(await error(service.create({ proposal_id: 3 }, caller(1)))).toEqual([
      'BadRequestError',
      'There is already an active pool for this proposal',
    ]);
  });

  it('close accepts only false (Δ31)', async () => {
    const { prisma, service } = setup();
    prisma.poll.findUnique.mockResolvedValue({ id: 1, proposal: { userId: 1 } });
    for (const v of [true, 'false', null, undefined]) {
      expect(await error(service.close('1', { is_poll_active: v }, caller(1)))).toEqual([
        'ValidationError',
        'Only closing a poll is supported',
      ]);
    }
    expect(prisma.poll.update).not.toHaveBeenCalled();
  });

  it('a vote whose counter update finds the poll closed rolls back as `Poll is not active`', async () => {
    const { prisma, service } = setup();
    prisma.poll.findUnique.mockResolvedValue({ id: 1, isActive: true });
    prisma.pollVote.findUnique.mockResolvedValue(null);
    prisma.pollVote.create.mockResolvedValue({ id: 1 });
    prisma.$executeRaw.mockResolvedValue(0); // closed between the check and the update
    expect(await error(service.createVote({ poll_id: 1, vote_result: true }, caller(2)))).toEqual([
      'BadRequestError',
      'Poll is not active',
    ]);
  });

  it('vote create forces user_id', async () => {
    const { prisma, service } = setup();
    prisma.poll.findUnique.mockResolvedValue({ id: 1, isActive: true });
    prisma.pollVote.findUnique.mockResolvedValue(null);
    prisma.pollVote.create.mockImplementation(({ data }: { data: Record<string, unknown> }) =>
      Promise.resolve({ id: 1, createdAt: T0, updatedAt: T0, ...data }),
    );
    const res = await service.createVote({ poll_id: '1', vote_result: false, user_id: 99 }, caller(2));
    expect(prisma.pollVote.create).toHaveBeenCalledWith({
      data: { pollId: 1, userId: 2, voteResult: false },
    });
    expect(res.data.attributes).toMatchObject({ poll_id: '1', user_id: '2', vote_result: false });
  });
});
