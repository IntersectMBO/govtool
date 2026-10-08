import { Controller, Get } from '@nestjs/common';
import { Public } from '../auth/auth.guard';
import { PrismaService } from '../prisma/prisma.service';
import { delegate, listEnvelope } from '../query/list';
import { parseQuery } from '../query/parse';
import { RawQuery } from '../query/raw-query';
import { scalarPaths, ResourceDef } from '../query/resource';
import { QueryAllowlist } from '../query/types';
import { LOOKUP_ROUTES } from './lookups.resources';

/** Scalars only, default sort `id asc`, no populate (§8.1). */
export function lookupAllowlist(resource: ResourceDef): QueryAllowlist {
  return { resource, filterable: scalarPaths(resource), sortable: scalarPaths(resource) };
}

const ROUTES = new Map(
  LOOKUP_ROUTES.map((r) => [r.path, { ...r, allowlist: lookupAllowlist(r.resource) }] as const),
);

/**
 * The worked example of a list route: parse the raw query against the
 * allowlist, run it through findList, serialize with the descriptor.
 */
@Controller()
@Public()
export class LookupsController {
  constructor(private readonly prisma: PrismaService) {}

  private list(path: string, raw: Record<string, unknown>) {
    const r = ROUTES.get(path)!;
    const q = parseQuery(raw, r.allowlist);
    return listEnvelope(delegate(this.prisma[r.model]), r.resource, q);
  }

  @Get('governance-action-types')
  governanceActionTypes(@RawQuery() q: Record<string, unknown>) {
    return this.list('governance-action-types', q);
  }
}
