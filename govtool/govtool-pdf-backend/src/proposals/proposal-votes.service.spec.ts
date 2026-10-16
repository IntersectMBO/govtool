import { ApiError } from '../common/errors';
import type { PrismaService } from '../prisma/prisma.service';
import { caller, prismaStub } from './__fixtures__/prisma-stub';
import { ProposalVotesService } from './proposal-votes.service';

const T0 = new Date('2026-09-26T10:00:00.000Z');

async function details(p: Promise<unknown>): Promise<unknown> {
  try {
    await p;
  } catch (e) {
    if (e instanceof ApiError)
      return e.message === 'Bad Request' ? e.details : `${e.errorName}: ${e.message}`;
    throw e;
  }
  throw new Error('expected an ApiError');
}

describe('ProposalVotesService', () => {
  const setup = () => {
    const prisma = prismaStub(['proposalVote', 'proposal']);
    return { prisma, service: new ProposalVotesService(prisma as unknown as PrismaService) };
  };

  it('findMine forces the caller over a client user_id (Δ29)', async () => {
    const { prisma, service } = setup();
    prisma.proposalVote.findFirst.mockResolvedValue(null);
    const res = await service.findMine({ filters: { proposal_id: { $eq: '4' }, user_id: '99' } }, caller(7));
    expect(res).toEqual({ data: null, meta: {} });
    expect(prisma.proposalVote.findFirst).toHaveBeenCalledWith({
      where: { AND: [{ proposalId: { equals: 4 } }, { userId: { equals: 7 } }] },
      orderBy: [{ createdAt: 'desc' }, { id: 'asc' }],
    });
  });

  it('create: errors in order, then forced user_id and +1 like', async () => {
    const { prisma, service } = setup();
    expect(await details(service.create({ proposal_id: 1 }, caller(1)))).toBe('Vote result is required');
    expect(await details(service.create({ vote_result: true }, caller(1)))).toBe('Proposal ID is required');
    prisma.proposal.findUnique.mockResolvedValue(null);
    expect(await details(service.create({ proposal_id: 1, vote_result: true }, caller(1)))).toBe(
      'Proposal not found',
    );
    prisma.proposal.findUnique.mockResolvedValue({ id: 1 });
    prisma.proposalVote.findUnique.mockResolvedValue({ id: 3 });
    expect(await details(service.create({ proposal_id: 1, vote_result: true }, caller(1)))).toBe(
      'Proposal vote for this user already exist',
    );
    prisma.proposalVote.findUnique.mockResolvedValue(null);
    prisma.proposalVote.create.mockImplementation(({ data }: { data: Record<string, unknown> }) =>
      Promise.resolve({ id: 1, createdAt: T0, updatedAt: T0, ...data }),
    );
    await service.create({ proposal_id: '1', vote_result: true, user_id: 42 }, caller(5));
    expect(prisma.proposalVote.create).toHaveBeenCalledWith({
      data: { proposalId: 1, userId: 5, voteResult: true },
    });
    expect(prisma.proposal.update).toHaveBeenCalledWith({
      where: { id: 1 },
      data: { likes: { increment: 1 } },
    });
  });

  it('update: owner check, then a conditional flip with one counter statement', async () => {
    const { prisma, service } = setup();
    prisma.proposalVote.findUnique.mockResolvedValue({ id: 3, userId: 5, proposalId: 1, voteResult: true });
    expect(await details(service.update('3', { vote_result: false }, caller(6)))).toBe(
      "ForbiddenError: You can't access this entry",
    );
    prisma.proposalVote.updateMany.mockResolvedValue({ count: 0 });
    expect(await details(service.update('3', { vote_result: true }, caller(5)))).toBe(
      'Proposal vote already updated',
    );
    expect(prisma.$executeRaw).not.toHaveBeenCalled();
    prisma.proposalVote.updateMany.mockResolvedValue({ count: 1 });
    prisma.proposalVote.findUniqueOrThrow.mockResolvedValue({
      id: 3,
      userId: 5,
      proposalId: 1,
      voteResult: false,
      createdAt: T0,
      updatedAt: T0,
    });
    await service.update('3', { vote_result: false }, caller(5));
    expect(prisma.proposalVote.updateMany).toHaveBeenLastCalledWith({
      where: { id: 3, voteResult: true },
      data: { voteResult: false },
    });
    expect(prisma.$executeRaw).toHaveBeenCalledTimes(1);
  });
});
