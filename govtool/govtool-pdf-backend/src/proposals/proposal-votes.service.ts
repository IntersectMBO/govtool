// SPEC §8.4 proposal votes (likes and dislikes).

import { Injectable } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { assertOwner } from '../auth/auth.guard';
import type { DataPayload } from '../common/body';
import { badRequestDetails, isUniqueViolation } from '../common/errors';
import { parseRouteId, readIntRef } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { andWith, removeTopLevel } from '../query/filter-helpers';
import { makeCond, parseQuery } from '../query/parse';
import { toPrismaOrderBy, toPrismaWhere } from '../query/prisma';
import { Entity, SingleEnvelope, serializeEntity, single } from '../query/serialize';
import { PROPOSAL_VOTES_ALLOWLIST } from './proposal.allowlists';
import { ProposalVoteResource } from './proposal.resources';

const FOREIGN = "You can't access this entry";

type Tx = Prisma.TransactionClient;

/** +1 on the side `like` names; with `flip`, −1 on the other (never below 0). */
async function bumpLikes(tx: Tx, proposalId: number, like: boolean, flip: boolean): Promise<void> {
  if (!flip) {
    await tx.proposal.update({
      where: { id: proposalId },
      data: like ? { likes: { increment: 1 } } : { dislikes: { increment: 1 } },
    });
  } else if (like) {
    await tx.$executeRaw`UPDATE proposals SET likes = likes + 1, dislikes = GREATEST(dislikes - 1, 0), updated_at = now() WHERE id = ${proposalId}`;
  } else {
    await tx.$executeRaw`UPDATE proposals SET dislikes = dislikes + 1, likes = GREATEST(likes - 1, 0), updated_at = now() WHERE id = ${proposalId}`;
  }
}

@Injectable()
export class ProposalVotesService {
  constructor(private readonly prisma: PrismaService) {}

  /** The caller's first matching vote, as a single object from a list route. */
  async findMine(raw: Record<string, unknown>, caller: AuthUser): Promise<SingleEnvelope<Entity | null>> {
    const q = parseQuery(raw, PROPOSAL_VOTES_ALLOWLIST);
    // A client user_id is dropped and the caller forced (Δ29).
    const filters = andWith(
      removeTopLevel(q.filters, 'user_id'),
      makeCond(ProposalVoteResource, ['user_id'], '$eq', caller.id),
    );
    const row = await this.prisma.proposalVote.findFirst({
      where: toPrismaWhere(ProposalVoteResource, filters),
      orderBy: toPrismaOrderBy(ProposalVoteResource, q.sort, [{ path: ['createdAt'], direction: 'desc' }]),
    });
    return single(row ? serializeEntity(row, ProposalVoteResource) : null);
  }

  async create(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    if (typeof d.vote_result !== 'boolean') throw badRequestDetails('Vote result is required');
    const like = d.vote_result;
    const proposalId = readIntRef(d, 'proposal_id');
    if (proposalId === undefined || proposalId === null) throw badRequestDetails('Proposal ID is required');
    const proposal = await this.prisma.proposal.findUnique({
      where: { id: proposalId },
      select: { id: true },
    });
    if (!proposal) throw badRequestDetails('Proposal not found');
    const exists = await this.prisma.proposalVote.findUnique({
      where: { proposalId_userId: { proposalId, userId: caller.id } },
      select: { id: true },
    });
    if (exists) throw badRequestDetails('Proposal vote for this user already exist');
    try {
      const vote = await this.prisma.$transaction(async (tx) => {
        const vote = await tx.proposalVote.create({
          data: { proposalId, userId: caller.id, voteResult: like },
        });
        await bumpLikes(tx, proposalId, like, false);
        return vote;
      });
      return single(serializeEntity(vote, ProposalVoteResource));
    } catch (e) {
      if (isUniqueViolation(e)) throw badRequestDetails('Proposal vote for this user already exist');
      throw e;
    }
  }

  async update(idRaw: string, d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const found = id === null ? null : await this.prisma.proposalVote.findUnique({ where: { id } });
    const vote = assertOwner(found, (v) => v.userId, caller, FOREIGN);
    if (typeof d.vote_result !== 'boolean') throw badRequestDetails('Vote result is required');
    const like = d.vote_result;
    const updated = await this.prisma.$transaction(async (tx) => {
      // Conditional on the stored side, so two concurrent flips move the
      // counters once.
      const res = await tx.proposalVote.updateMany({
        where: { id: vote.id, voteResult: !like },
        data: { voteResult: like },
      });
      if (res.count === 0) throw badRequestDetails('Proposal vote already updated');
      await bumpLikes(tx, vote.proposalId, like, true);
      return tx.proposalVote.findUniqueOrThrow({ where: { id: vote.id } });
    });
    return single(serializeEntity(updated, ProposalVoteResource));
  }
}
