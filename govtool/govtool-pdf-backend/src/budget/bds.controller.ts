// SPEC §8.8 budget discussions.

import { Controller, HttpCode, Delete, Get, Param, Post } from '@nestjs/common';
import type { AuthUser } from '../auth/auth-user';
import { Caller, Public } from '../auth/auth.guard';
import { DataBody } from '../common/body';
import type { DataPayload } from '../common/body';
import { notFound } from '../common/errors';
import { parseRouteId } from '../common/fields';
import { PrismaService } from '../prisma/prisma.service';
import { listEnvelope } from '../query/list';
import { parsePopulate, parseQuery } from '../query/parse';
import { toPrismaInclude, toPrismaOrderBy, toPrismaWhere } from '../query/prisma';
import { RawQuery } from '../query/raw-query';
import { serializeEntity, single } from '../query/serialize';
import { SortItem } from '../query/types';
import { parseBdInput } from './bd-input';
import { bdCreateResponse, bdExtra, bdListDelegate, withComputedInclude } from './bd-serialize';
import { BDS_ALLOWLIST } from './budget.allowlists';
import { BdResource } from './budget.resources';
import { BdsService } from './bds.service';

/** The fixed populate of GET /api/bd/versions/:id. */
const VERSIONS_POPULATE = parsePopulate(
  [
    'creator',
    'bd_costing.preferred_currency',
    'bd_proposal_detail.contract_type_name',
    'bd_further_information',
    'bd_psapb.type_name',
    'bd_psapb.roadmap_name',
    'bd_psapb.committee_name',
    'bd_proposal_ownership.be_country',
  ],
  BDS_ALLOWLIST,
);

const NEWEST_FIRST: SortItem[] = [{ path: ['createdAt'], direction: 'desc' }];

@Controller()
export class BdsController {
  constructor(
    private readonly prisma: PrismaService,
    private readonly bds: BdsService,
  ) {}

  @Get('bds')
  @Public()
  list(@RawQuery() raw: Record<string, unknown>) {
    const q = parseQuery(raw, BDS_ALLOWLIST);
    return listEnvelope(bdListDelegate(this.prisma.bd), BdResource, q, { extra: bdExtra });
  }

  /** `:id` is a master id; answers the active version (404 `Not Found`, Δ35). */
  @Get('bds/:id')
  @Public()
  async findOne(@Param('id') rawId: string, @RawQuery() raw: Record<string, unknown>) {
    const q = parseQuery(raw, BDS_ALLOWLIST);
    const masterId = parseRouteId(rawId);
    if (masterId === null) throw notFound();
    const clientWhere = toPrismaWhere(BdResource, q.filters);
    const forced = { masterId, isActive: true };
    const row = await this.prisma.bd.findFirst({
      where: Object.keys(clientWhere).length ? { AND: [forced, clientWhere] } : forced,
      orderBy: toPrismaOrderBy(BdResource, q.sort),
      include: withComputedInclude(toPrismaInclude(BdResource, q.populate)),
    });
    if (!row) throw notFound();
    return single(
      serializeEntity(row, BdResource, { populate: q.populate, fields: q.fields, extra: bdExtra(row) }),
    );
  }

  /** Every version of a chain, newest first, fixed populate, no pagination. */
  @Get('bd/versions/:id')
  @Public()
  async versions(@Param('id') rawId: string) {
    const masterId = parseRouteId(rawId);
    if (masterId === null) return { data: [], meta: {} };
    const rows = await this.prisma.bd.findMany({
      where: { masterId },
      orderBy: toPrismaOrderBy(BdResource, NEWEST_FIRST),
      include: withComputedInclude(toPrismaInclude(BdResource, VERSIONS_POPULATE)),
    });
    return {
      data: rows.map((row) =>
        serializeEntity(row, BdResource, { populate: VERSIONS_POPULATE, extra: bdExtra(row) }),
      ),
      meta: {},
    };
  }

  /** Raw, not enveloped (§8.8). */
  @Post('bds')
  @HttpCode(200)
  async create(@Caller() caller: AuthUser, @DataBody() data: DataPayload) {
    const input = parseBdInput(data);
    return bdCreateResponse(await this.bds.create(input, caller));
  }

  /** `:id` is a row id; deletes the whole chain. */
  @Delete('bds/:id')
  async remove(@Caller() caller: AuthUser, @Param('id') rawId: string) {
    const id = parseRouteId(rawId);
    if (id === null) throw notFound();
    const row = await this.bds.deleteChain(id, caller);
    return single(serializeEntity(row, BdResource));
  }
}
