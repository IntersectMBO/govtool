import { ApiError } from '../common/errors';
import type { PrismaService } from '../prisma/prisma.service';
import { caller, prismaStub } from '../proposals/__fixtures__/prisma-stub';
import { CommentsService, newReportHash } from './comments.service';

const T0 = new Date('2026-09-26T10:00:00.000Z');

async function details(p: Promise<unknown>): Promise<unknown> {
  try {
    await p;
  } catch (e) {
    if (e instanceof ApiError)
      return e.errorName === 'BadRequestError' && e.message === 'Bad Request' ? e.details : e.message;
    throw e;
  }
  throw new Error('expected an ApiError');
}

describe('CommentsService', () => {
  const setup = () => {
    const prisma = prismaStub(['comment', 'proposal', 'commentsReport']);
    const service = new CommentsService(prisma as unknown as PrismaService);
    return { prisma, service };
  };

  it('validates before touching the database', async () => {
    const { prisma, service } = setup();
    const me = caller(1);
    expect(await details(service.create({ proposal_id: 1 }, me))).toBe('Comment text is required');
    expect(await details(service.create({ proposal_id: 1, comment_text: '   ' }, me))).toBe(
      'Comment text is required',
    );
    expect(await details(service.create({ comment_text: 'x' }, me))).toBe('Proposal ID is required');
    expect(await details(service.create({ comment_text: 'x', proposal_id: '' }, me))).toBe(
      'Proposal ID is required',
    );
    // Budget discussions are gone (D167): a BD target is no target.
    expect(await details(service.create({ comment_text: 'x', bd_proposal_id: 2 }, me))).toBe(
      'Proposal ID is required',
    );
    expect(prisma.proposal.findUnique).not.toHaveBeenCalled();
    expect(prisma.$transaction).not.toHaveBeenCalled();
  });

  it('forces user_id and drep_id from the token; counts the comment on the proposal', async () => {
    const { prisma, service } = setup();
    prisma.proposal.findUnique.mockResolvedValue({ id: 5 });
    prisma.comment.create.mockImplementation(({ data }: { data: Record<string, unknown> }) =>
      Promise.resolve({ id: 9, createdAt: T0, updatedAt: T0, ...data }),
    );
    const res = await service.create(
      { proposal_id: '5', comment_text: 'hi', user_id: 99, drep_id: 'client', comment_parent_id: '' },
      caller(3, 'd'.repeat(56)),
    );
    expect(prisma.comment.create).toHaveBeenCalledWith({
      data: { proposalId: 5, parentId: null, userId: 3, text: 'hi', drepId: 'd'.repeat(56) },
    });
    expect(prisma.proposal.update).toHaveBeenCalledWith({
      where: { id: 5 },
      data: { commentsNumber: { increment: 1 } },
    });
    expect(res.data.attributes).toMatchObject({ user_id: '3', drep_id: 'd'.repeat(56), proposal_id: '5' });
    expect(res.data.attributes).not.toHaveProperty('user_govtool_username');
  });

  it('report forces the reporter and a fresh hash, whatever the body says', async () => {
    const { prisma, service } = setup();
    prisma.comment.findUnique.mockResolvedValue({ id: 4 });
    prisma.commentsReport.findUnique.mockResolvedValue(null);
    prisma.commentsReport.create.mockImplementation(({ data }: { data: Record<string, unknown> }) =>
      Promise.resolve({ id: 1, createdAt: T0, updatedAt: T0, ...data }),
    );
    const res = await service.report(
      { comment: 4, reporter: 99, moderator: 98, moderation_status: true, hash: 'x' },
      caller(2),
    );
    const data = (prisma.commentsReport.create.mock.calls[0] as [{ data: Record<string, unknown> }])[0].data;
    expect(data).toMatchObject({ commentId: 4, reporterId: 2, moderatorId: null, moderationStatus: null });
    expect(data.hash).toMatch(/^[A-Za-z0-9]{89}$/);
    expect(Object.keys(res.data.attributes).sort()).toEqual([
      'createdAt',
      'moderation_status',
      'publishedAt',
      'updatedAt',
    ]);
  });
});

describe('newReportHash', () => {
  it('is 89 characters of [A-Za-z0-9] and does not repeat', () => {
    const hashes = new Set(Array.from({ length: 200 }, () => newReportHash()));
    expect(hashes.size).toBe(200);
    for (const h of hashes) expect(h).toMatch(/^[A-Za-z0-9]{89}$/);
  });
});
