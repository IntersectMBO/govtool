// SPEC §8.10 BD drafts. Authenticated and scoped to the caller: another
// user's draft answers exactly like a missing one.

import { Controller, HttpCode, Delete, Get, Param, Post, Put } from '@nestjs/common';
import { Prisma } from '@prisma/client';
import type { AuthUser } from '../auth/auth-user';
import { Caller } from '../auth/auth.guard';
import { DataBody } from '../common/body';
import type { DataPayload } from '../common/body';
import { notFound, validationError } from '../common/errors';
import { hasNulDeep, parseRouteId } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { delegate, listEnvelope } from '../query/list';
import { parseQuery } from '../query/parse';
import { RawQuery } from '../query/raw-query';
import { serializeEntity, single } from '../query/serialize';
import { BD_DRAFTS_ALLOWLIST } from './budget.allowlists';
import { BdDraftResource } from './budget.resources';

const UPDATE_MISSING = "Resource not found or you don't have permission to update it";
const DELETE_MISSING = "Resource not found or you don't have permission to delete it";

function readDraftData(d: DataPayload): Prisma.InputJsonObject {
  const v = d.draft_data;
  if (typeof v !== 'object' || v === null || Array.isArray(v) || hasNulDeep(v)) {
    throw validationError('draft_data is invalid');
  }
  return v;
}

@Controller('bd-drafts')
export class BdDraftsController {
  constructor(private readonly prisma: PrismaService) {}

  @Get()
  list(@Caller() caller: AuthUser, @RawQuery() raw: Record<string, unknown>) {
    const q = parseQuery(raw, BD_DRAFTS_ALLOWLIST);
    return listEnvelope(delegate(this.prisma.bdDraft), BdDraftResource, q, {
      where: { creatorId: caller.id },
    });
  }

  @Post()
  @HttpCode(200)
  async create(@Caller() caller: AuthUser, @DataBody() data: DataPayload) {
    const draftData = readDraftData(data);
    const row = await this.prisma.bdDraft.create({ data: { creatorId: caller.id, draftData } });
    return single(serializeEntity(row, BdDraftResource));
  }

  @Put(':id')
  async update(@Caller() caller: AuthUser, @Param('id') rawId: string, @DataBody() data: DataPayload) {
    const id = parseRouteId(rawId);
    if (id === null) throw notFound(UPDATE_MISSING);
    const draftData = readDraftData(data);
    return this.prisma.$transaction(async (tx) => {
      const res = await tx.bdDraft.updateMany({ where: { id, creatorId: caller.id }, data: { draftData } });
      if (res.count === 0) throw notFound(UPDATE_MISSING);
      const row = await tx.bdDraft.findUniqueOrThrow({ where: { id } });
      return single(serializeEntity(row, BdDraftResource));
    });
  }

  @Delete(':id')
  async remove(@Caller() caller: AuthUser, @Param('id') rawId: string) {
    const id = parseRouteId(rawId);
    if (id === null) throw notFound(DELETE_MISSING);
    return this.prisma.$transaction(async (tx) => {
      const row = await tx.bdDraft.findFirst({ where: { id, creatorId: caller.id } });
      if (!row) throw notFound(DELETE_MISSING);
      await tx.bdDraft.delete({ where: { id } });
      return single(serializeEntity(row, BdDraftResource));
    });
  }
}
