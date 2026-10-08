// SPEC §8.7 comments and §8.12 reports.

import { randomInt } from 'node:crypto';
import { Injectable } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { assertOwner } from '../auth/auth.guard';
import type { DataPayload } from '../common/body';
import { badRequest, badRequestDetails, isUniqueViolation, validationError } from '../common/errors';
import { hasNul, parseRouteId, toIntRef } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { parseQuery } from '../query/parse';
import { toPrismaInclude, toPrismaOrderBy, toPrismaPaging, toPrismaWhere } from '../query/prisma';
import {
  Entity,
  ListEnvelope,
  SingleEnvelope,
  list,
  paginationMeta,
  serializeEntity,
  single,
} from '../query/serialize';
import { COMMENTS_ALLOWLIST } from './comment.allowlists';
import { CommentResource, CommentsReportResource } from './comment.resources';

export const COMMENT_TEXT_MAX = 15000;
export const REPORT_HASH_LENGTH = 89;
const HASH_ALPHABET = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';

/** 89 characters of [A-Za-z0-9] from the CSPRNG (§8.12). */
export function newReportHash(): string {
  let s = '';
  for (let i = 0; i < REPORT_HASH_LENGTH; i++) s += HASH_ALPHABET[randomInt(HASH_ALPHABET.length)];
  return s;
}

/** A present, non-empty reference, else undefined. Type errors are V. */
function readRef(d: DataPayload, field: string): number | undefined {
  const v = d[field];
  if (v === undefined || v === null || v === '') return undefined;
  return toIntRef(v, field);
}

@Injectable()
export class CommentsService {
  constructor(private readonly prisma: PrismaService) {}

  // ------------------------------------------------------------- comments

  async list(raw: Record<string, unknown>): Promise<ListEnvelope<Entity>> {
    const q = parseQuery(raw, COMMENTS_ALLOWLIST);
    const where = toPrismaWhere(CommentResource, q.filters) as Prisma.CommentWhereInput;
    const { skip, take } = toPrismaPaging(q.pagination);
    const include = {
      ...(toPrismaInclude(CommentResource, q.populate) ?? {}),
      user: { select: { govtoolUsername: true, isValidated: true } },
      _count: { select: { replies: true } },
    } as const;
    const [rows, total] = await Promise.all([
      this.prisma.comment.findMany({
        where,
        orderBy: toPrismaOrderBy(CommentResource, q.sort),
        skip,
        take,
        include,
      }),
      q.pagination.withCount ? this.prisma.comment.count({ where }) : Promise.resolve(null),
    ]);
    const data = rows.map((row) =>
      serializeEntity(row, CommentResource, {
        populate: q.populate,
        fields: q.fields,
        extra: {
          user_govtool_username: row.user.govtoolUsername ?? 'Anonymous',
          user_is_validated: row.user.isValidated,
          subcommens_number: row._count.replies,
        },
      }),
    );
    return list(data, paginationMeta(q.pagination, total));
  }

  async create(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const text = d.comment_text;
    if (typeof text !== 'string' || text.trim().length === 0 || text.length > COMMENT_TEXT_MAX) {
      throw badRequestDetails('Comment text is required');
    }
    if (hasNul(text)) throw validationError('comment_text is invalid');
    const proposalId = readRef(d, 'proposal_id');
    if (proposalId === undefined) throw badRequestDetails('Proposal ID is required');
    const p = await this.prisma.proposal.findUnique({ where: { id: proposalId }, select: { id: true } });
    if (!p) throw badRequestDetails('Proposal not found');
    const parentId = readRef(d, 'comment_parent_id');
    if (parentId !== undefined) {
      // The parent must sit on the same proposal (Δ34).
      const parent = await this.prisma.comment.findFirst({
        where: { id: parentId, proposalId },
        select: { id: true },
      });
      if (!parent) throw badRequestDetails('Parent comment not found');
    }
    const comment = await this.prisma.$transaction(async (tx) => {
      const comment = await tx.comment.create({
        data: {
          proposalId,
          parentId: parentId ?? null,
          userId: caller.id,
          text,
          // From the token only; pdf-ui's drep_id is ignored.
          drepId: caller.dRepID ?? null,
        },
      });
      await tx.proposal.update({
        where: { id: proposalId },
        data: { commentsNumber: { increment: 1 } },
      });
      return comment;
    });
    return single(serializeEntity(comment, CommentResource));
  }

  // ------------------------------------------------------------- reports

  async report(d: DataPayload, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const commentId = readRef(d, 'comment');
    if (commentId === undefined) throw badRequest('Comment is mandatory.');
    const comment = await this.prisma.comment.findUnique({ where: { id: commentId }, select: { id: true } });
    if (!comment) throw badRequest('Comment not found');
    const exists = await this.prisma.commentsReport.findUnique({
      where: { commentId_reporterId: { commentId, reporterId: caller.id } },
      select: { id: true },
    });
    if (exists) throw badRequest('Comment already reported');
    try {
      // Reporter forced (Δ42); moderation fields untouched by clients.
      const report = await this.prisma.commentsReport.create({
        data: {
          commentId,
          reporterId: caller.id,
          moderatorId: null,
          moderationStatus: null,
          hash: newReportHash(),
          publishedAt: new Date(),
        },
      });
      return single(serializeEntity(report, CommentsReportResource));
    } catch (e) {
      if (isUniqueViolation(e)) throw badRequest('Comment already reported');
      throw e;
    }
  }

  async removeReport(idRaw: string, caller: AuthUser): Promise<SingleEnvelope<Entity>> {
    const id = parseRouteId(idRaw);
    const found = id === null ? null : await this.prisma.commentsReport.findUnique({ where: { id } });
    const report = assertOwner(found, (r) => r.reporterId, caller, "You can't access this entry");
    await this.prisma.commentsReport.delete({ where: { id: report.id } });
    return single(serializeEntity(report, CommentsReportResource));
  }
}
