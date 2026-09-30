// SPEC §8.5 polls and §8.6 poll votes.

import { Injectable } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { assertOwner } from '../auth/auth.guard';
import type { DataPayload } from '../common/body';
import { badRequestDetails, forbidden, isUniqueViolation, validationError } from '../common/errors';
import { parseRouteId, toIntRef } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { removeTopLevel } from '../query/filter-helpers';
import { delegate, listEnvelope } from '../query/list';
import { parseQuery } from '../query/parse';
import { Entity, ListEnvelope, SingleEnvelope, serializeEntity, single } from '../query/serialize';
import { POLL_VOTES_ALLOWLIST, POLLS_ALLOWLIST } from './poll.allowlists';
import { PollResource, PollVoteResource } from './poll.resources';

type Tx = Prisma.TransactionClient;

const ACTIVE_EXISTS = 'There is already an active pool for this proposal';
const NOT_ACTIVE = 'Poll is not active';

/** A present, non-empty reference, else undefined. Type errors are V. */
function readRef(d: DataPayload, field: string): number | undefined {
  const v = d[field];
  if (v === undefined || v === null || v === '') return undefined;
  return toIntRef(v, field);
}

/**
 * Move the poll counters, only while the poll is active; false when it is
 * not (the caller rolls back). `flip` also takes one from the other side.
 */
async function bumpPoll(tx: Tx, pollId: number, yes: boolean, flip: boolean): Promise<boolean> {
  let n: number;
  if (!flip) {
    n = yes
      ? await tx.$executeRaw`UPDATE polls SET yes = yes + 1, updated_at = now() WHERE id = ${pollId} AND is_active`
      : await tx.$executeRaw`UPDATE polls SET no = no + 1, updated_at = now() WHERE id = ${pollId} AND is_active`;
  } else {
    n = yes
      ? await tx.$executeRaw`UPDATE polls SET yes = yes + 1, no = GREATEST(no - 1, 0), updated_at = now() WHERE id = ${pollId} AND is_active`
      : await tx.$executeRaw`UPDATE polls SET no = no + 1, yes = GREATEST(yes - 1, 0), updated_at = now() WHERE id = ${pollId} AND is_active`;
  }
  return n === 1;
}

@Injectable()
export class PollsService {
  constructor(private readonly prisma: PrismaService) {}

  // ------------------------------------------------------------- polls

  list(raw: Record<string, unknown>): Promise<ListEnvelope<Entity>> {
    return listEnvelope(delegate(this.prisma.poll), PollResource, parseQuery(raw, POLLS_ALLOWLIST));
  }

  async create(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const proposalId = readRef(d, 'proposal_id');
    const proposal =
      proposalId === undefined ? null : await this.prisma.proposal.findUnique({ where: { id: proposalId } });
    if (!proposal) throw badRequestDetails('Proposal not found');
    if (proposal.userId !== caller.id) throw forbidden('User is not owner of this proposal');
    const active = await this.prisma.poll.count({ where: { proposalId: proposal.id, isActive: true } });
    if (active > 0) throw badRequestDetails(ACTIVE_EXISTS);
    try {
      // Every field but the proposal is server-set (Δ30); the partial unique
      // index settles a race between two creates.
      const poll = await this.prisma.poll.create({
        data: { proposalId: proposal.id, isActive: true, yes: 0, no: 0, startDt: new Date() },
      });
      return single(serializeEntity(poll, PollResource));
    } catch (e) {
      if (isUniqueViolation(e)) throw badRequestDetails(ACTIVE_EXISTS);
      throw e;
    }
  }

  async close(idRaw: string, d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const poll =
      id === null ? null : await this.prisma.poll.findUnique({ where: { id }, include: { proposal: true } });
    if (!poll) throw badRequestDetails('Poll not found');
    if (poll.proposal.userId !== caller.id) throw forbidden('User is not authorized to update this Poll.');
    // Reopening could create a second active poll (Δ31).
    if (d.is_poll_active !== false) throw validationError('Only closing a poll is supported');
    const updated = await this.prisma.poll.update({ where: { id: poll.id }, data: { isActive: false } });
    return single(serializeEntity(updated, PollResource));
  }

  // ------------------------------------------------------------- poll votes

  listVotes(raw: Record<string, unknown>, caller: AuthUser): Promise<ListEnvelope<Entity>> {
    const q = parseQuery(raw, POLL_VOTES_ALLOWLIST);
    return listEnvelope(
      delegate(this.prisma.pollVote),
      PollVoteResource,
      { ...q, filters: removeTopLevel(q.filters, 'user_id') },
      { where: { userId: caller.id } },
    );
  }

  async createVote(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    if (typeof d.vote_result !== 'boolean') throw badRequestDetails('Vote result is required');
    const yes = d.vote_result;
    const pollId = readRef(d, 'poll_id');
    if (pollId === undefined) throw badRequestDetails('Poll ID is required');
    const poll = await this.prisma.poll.findUnique({ where: { id: pollId } });
    if (!poll) throw badRequestDetails('Poll not found');
    if (!poll.isActive) throw badRequestDetails(NOT_ACTIVE);
    const exists = await this.prisma.pollVote.findUnique({
      where: { pollId_userId: { pollId, userId: caller.id } },
      select: { id: true },
    });
    if (exists) throw badRequestDetails('Poll vote for this user already exist');
    try {
      const vote = await this.prisma.$transaction(async (tx) => {
        const vote = await tx.pollVote.create({ data: { pollId, userId: caller.id, voteResult: yes } });
        // Only active polls take votes (Δ32), checked again under the row lock.
        if (!(await bumpPoll(tx, pollId, yes, false))) throw badRequestDetails(NOT_ACTIVE);
        return vote;
      });
      return single(serializeEntity(vote, PollVoteResource));
    } catch (e) {
      if (isUniqueViolation(e)) throw badRequestDetails('Poll vote for this user already exist');
      throw e;
    }
  }

  async updateVote(idRaw: string, d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const found = id === null ? null : await this.prisma.pollVote.findUnique({ where: { id } });
    const vote = assertOwner(found, (v) => v.userId, caller, "You can't access this entry");
    if (typeof d.vote_result !== 'boolean') throw badRequestDetails('Vote result is required');
    const yes = d.vote_result;
    const updated = await this.prisma.$transaction(async (tx) => {
      const res = await tx.pollVote.updateMany({
        where: { id: vote.id, voteResult: !yes },
        data: { voteResult: yes },
      });
      if (res.count === 0) throw badRequestDetails('Poll vote already updated');
      if (!(await bumpPoll(tx, vote.pollId, yes, true))) throw badRequestDetails(NOT_ACTIVE);
      return tx.pollVote.findUniqueOrThrow({ where: { id: vote.id } });
    });
    return single(serializeEntity(updated, PollVoteResource));
  }
}
