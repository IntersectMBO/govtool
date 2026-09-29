// SPEC §8.11 BD polls and BD poll votes. Polls are created with their BD
// and closed by an operator; votes are DRep-only, with `user_id` and
// `drep_id` taken from the token.

import { Controller, HttpCode, Get, Param, Post, Put } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { assertOwner, Caller, Public } from '../auth/auth.guard';
import { DataBody } from '../common/body';
import type { DataPayload } from '../common/body';
import { badRequest, isUniqueViolation, notFound, validationError } from '../common/errors';
import { parseRouteId, readIntRef, readString } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { delegate, listEnvelope } from '../query/list';
import { parseQuery } from '../query/parse';
import { RawQuery } from '../query/raw-query';
import { serializeEntity, single } from '../query/serialize';
import { chainSubmitted } from './bds.service';
import { BD_POLL_VOTES_ALLOWLIST, BD_POLLS_ALLOWLIST } from './budget.allowlists';
import { BdPollResource, BdPollVoteResource } from './budget.resources';

type Tx = Prisma.TransactionClient;

export const CREATE_VOTE_LOCKED =
  'Creating poll votes is not allowed after the proposal has been submitted for voting.';
export const MODIFY_VOTE_LOCKED =
  'Modifying poll votes is not allowed after the proposal has been submitted for voting.';

/** Lock the poll row so a vote and an operator close cannot interleave. */
async function lockPoll(tx: Tx, pollId: number) {
  const rows = await tx.$queryRaw<Array<{ id: number; bd_master_id: number; is_active: boolean }>>`
    SELECT id, bd_master_id, is_active FROM bd_polls WHERE id = ${pollId} FOR UPDATE`;
  return rows[0] ?? null;
}

function readVoteResult(d: DataPayload): boolean {
  if (typeof d.vote_result !== 'boolean') throw badRequest('Vote result is required');
  return d.vote_result;
}

@Controller()
export class BdPollsController {
  constructor(private readonly prisma: PrismaService) {}

  @Get('bd-polls')
  @Public()
  listPolls(@RawQuery() raw: Record<string, unknown>) {
    return listEnvelope(delegate(this.prisma.bdPoll), BdPollResource, parseQuery(raw, BD_POLLS_ALLOWLIST));
  }

  /** Public by design: pdf-ui lists the DReps who voted. */
  @Get('bd-poll-votes')
  @Public()
  listVotes(@RawQuery() raw: Record<string, unknown>) {
    return listEnvelope(
      delegate(this.prisma.bdPollVote),
      BdPollVoteResource,
      parseQuery(raw, BD_POLL_VOTES_ALLOWLIST),
    );
  }

  @Post('bd-poll-votes')
  @HttpCode(200)
  async createVote(@Caller() caller: AuthUser, @DataBody() data: DataPayload) {
    if (!caller.dRepID) throw badRequest('Missing dRepID');
    const drepId = caller.dRepID;
    const voteResult = readVoteResult(data);
    const pollId = readIntRef(data, 'bd_poll_id');
    if (pollId === null || pollId === undefined) throw badRequest('Poll ID is required');
    const power = readString(data, 'drep_voting_power');
    const drepVotingPower = power === null || power === undefined || power === '' ? '0' : power;

    try {
      const vote = await this.prisma.$transaction(async (tx) => {
        const poll = await lockPoll(tx, pollId);
        if (!poll) throw badRequest('Poll not found');
        if (!poll.is_active) throw badRequest('Poll is not active');
        if (await chainSubmitted(tx, poll.bd_master_id)) throw validationError(CREATE_VOTE_LOCKED);
        const created = await tx.bdPollVote.create({
          data: { bdPollId: pollId, userId: caller.id, drepId, voteResult, drepVotingPower },
        });
        await tx.bdPoll.update({
          where: { id: pollId },
          data: voteResult ? { yes: { increment: 1 } } : { no: { increment: 1 } },
        });
        return created;
      });
      return single(serializeEntity(vote, BdPollVoteResource));
    } catch (e) {
      // Either unique index: (poll, user) or (poll, DRep) (Δ12).
      if (isUniqueViolation(e)) throw badRequest('Poll vote for this user already exist');
      throw e;
    }
  }

  @Put('bd-poll-votes/:id')
  async updateVote(@Caller() caller: AuthUser, @Param('id') rawId: string, @DataBody() data: DataPayload) {
    const id = parseRouteId(rawId);
    if (id === null) throw notFound();
    const existing = assertOwner(
      await this.prisma.bdPollVote.findUnique({ where: { id } }),
      (v) => v.userId,
      caller,
      "You can't access this entry",
    );
    const voteResult = readVoteResult(data);
    if (existing.voteResult === voteResult) throw badRequest('Poll vote already updated');
    const vote = await this.prisma.$transaction(async (tx) => {
      const poll = await lockPoll(tx, existing.bdPollId);
      if (!poll) throw notFound();
      if (!poll.is_active) throw badRequest('Poll is not active');
      if (await chainSubmitted(tx, poll.bd_master_id)) throw validationError(MODIFY_VOTE_LOCKED);
      // Conditional on the stored value, so a concurrent flip cannot count twice.
      const flipped = await tx.bdPollVote.updateMany({
        where: { id, voteResult: !voteResult },
        data: { voteResult },
      });
      if (flipped.count === 0) throw badRequest('Poll vote already updated');
      if (voteResult) {
        await tx.$executeRaw`UPDATE bd_polls SET "yes" = "yes" + 1, "no" = GREATEST("no" - 1, 0), updated_at = now() WHERE id = ${poll.id}`;
      } else {
        await tx.$executeRaw`UPDATE bd_polls SET "no" = "no" + 1, "yes" = GREATEST("yes" - 1, 0), updated_at = now() WHERE id = ${poll.id}`;
      }
      return tx.bdPollVote.findUniqueOrThrow({ where: { id } });
    });
    return single(serializeEntity(vote, BdPollVoteResource));
  }
}
