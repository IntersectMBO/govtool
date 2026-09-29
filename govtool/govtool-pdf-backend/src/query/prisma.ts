// ParsedQuery pieces to Prisma `where` / `orderBy` / `include` / paging args.

import { ResourceDef, ScalarDef, resolveRelation, resolveScalar } from './resource';
import { CondNode, FilterNode, Pagination, PopulateTree, SortItem } from './types';

export type PrismaWhere = Record<string, unknown>;
export type PrismaOrderBy = Record<string, unknown>;

/**
 * Matches nothing / everything, as Prisma model-level filters. Not `{OR: []}`
 * and `{AND: []}`: Prisma drops those when they are nested in another
 * AND/OR, so a nested FALSE matched everything. Every model has `id`.
 */
const FALSE: PrismaWhere = { id: { in: [] } };
const TRUE: PrismaWhere = { id: { notIn: [] } };

/** Escape LIKE wildcards: Prisma passes `contains` values through unescaped. */
export function escapeLike(s: string): string {
  return s.replace(/[\\%_]/g, (c) => `\\${c}`);
}

function leaf(field: string, def: ScalarDef, c: CondNode): PrismaWhere {
  const v = c.value;
  const nullable = def.nullable !== false;
  const on = (filter: unknown): PrismaWhere => ({ [field]: filter });
  switch (c.op) {
    case '$eq':
      return on({ equals: v });
    case '$ne':
      return on({ not: v });
    case '$lt':
      return on({ lt: v });
    case '$lte':
      return on({ lte: v });
    case '$gt':
      return on({ gt: v });
    case '$gte':
      return on({ gte: v });
    // Prisma's boolean filter has no in/notIn: spell them out.
    case '$in':
      if (def.type !== 'boolean') return on({ in: v });
      return (v as unknown[]).length ? { OR: (v as unknown[]).map((x) => on({ equals: x })) } : FALSE;
    case '$notIn':
      if (def.type !== 'boolean') return on({ notIn: v });
      return (v as unknown[]).length ? { AND: (v as unknown[]).map((x) => on({ not: x })) } : TRUE;
    case '$null':
    case '$notNull': {
      const wantNull = c.op === '$null' ? v === true : v !== true;
      if (!nullable) return wantNull ? FALSE : TRUE;
      return on(wantNull ? { equals: null } : { not: null });
    }
    case '$contains':
      return on({ contains: escapeLike(v as string) });
    case '$notContains':
      return { NOT: on({ contains: escapeLike(v as string) }) };
    case '$containsi':
      return on({ contains: escapeLike(v as string), mode: 'insensitive' });
    case '$notContainsi':
      return { NOT: on({ contains: escapeLike(v as string), mode: 'insensitive' }) };
    case '$startsWith':
      return on({ startsWith: escapeLike(v as string) });
    case '$endsWith':
      return on({ endsWith: escapeLike(v as string) });
  }
}

function condToWhere(resource: ResourceDef, c: CondNode): PrismaWhere {
  const hops: Array<{ field: string; many: boolean }> = [];
  let r = resource;
  const relSegs = c.path.slice(0, -1);
  const last = c.path[c.path.length - 1];
  for (let i = 0; i < relSegs.length; i++) {
    const rel = resolveRelation(r, relSegs[i]);
    if (!rel) throw new Error(`Unresolvable filter path ${c.path.join('.')} on ${resource.name}`);
    // `<to-one relation>.id` compares the FK on the owning side.
    if (i === relSegs.length - 1 && last === 'id' && !rel.many && rel.fk) {
      const inner = leaf(rel.fk, { field: rel.fk, type: 'int', nullable: rel.nullable }, c);
      return wrap(hops, inner);
    }
    hops.push({ field: rel.field, many: rel.many });
    r = rel.target();
  }
  const s = resolveScalar(r, last);
  if (!s) {
    throw new Error(
      `Unresolvable filter path ${c.path.join('.')} on ${resource.name} (virtual filters must be rewritten)`,
    );
  }
  return wrap(hops, leaf(s.field, s, c));
}

function wrap(hops: Array<{ field: string; many: boolean }>, inner: PrismaWhere): PrismaWhere {
  let w = inner;
  for (let i = hops.length - 1; i >= 0; i--) {
    const h = hops[i];
    w = { [h.field]: h.many ? { some: w } : { is: w } };
  }
  return w;
}

export function toPrismaWhere(resource: ResourceDef, node: FilterNode | null): PrismaWhere {
  if (!node) return {};
  switch (node.kind) {
    case 'and':
      if (node.children.length === 0) return TRUE;
      if (node.children.length === 1) return toPrismaWhere(resource, node.children[0]);
      return { AND: node.children.map((c) => toPrismaWhere(resource, c)) };
    case 'or':
      return { OR: node.children.map((c) => toPrismaWhere(resource, c)) };
    case 'not':
      return { NOT: toPrismaWhere(resource, node.child) };
    case 'cond':
      return condToWhere(resource, node);
  }
}

/** Client sort (or `fallback` when none), then `id asc` as the tie-breaker. */
export function toPrismaOrderBy(
  resource: ResourceDef,
  sort: SortItem[],
  fallback: SortItem[] = [],
): PrismaOrderBy[] {
  const items = sort.length ? sort : fallback;
  const out: PrismaOrderBy[] = [];
  for (const it of items) {
    let r = resource;
    const fields: string[] = [];
    for (const seg of it.path.slice(0, -1)) {
      const rel = resolveRelation(r, seg);
      if (!rel || rel.many) throw new Error(`Unsortable path ${it.path.join('.')}`);
      fields.push(rel.field);
      r = rel.target();
    }
    const s = resolveScalar(r, it.path[it.path.length - 1]);
    if (!s) throw new Error(`Unsortable path ${it.path.join('.')}`);
    let o: PrismaOrderBy = { [s.field]: it.direction };
    for (let i = fields.length - 1; i >= 0; i--) o = { [fields[i]]: o };
    out.push(o);
  }
  const lastItem = items[items.length - 1];
  if (!(lastItem && lastItem.path.length === 1 && lastItem.path[0] === 'id')) {
    out.push({ id: 'asc' });
  }
  return out;
}

function componentIncludes(resource: ResourceDef): Record<string, unknown> {
  const inc: Record<string, unknown> = {};
  for (const c of Object.values(resource.components ?? {})) {
    inc[c.field] = { orderBy: [{ position: 'asc' }, { id: 'asc' }] };
  }
  return inc;
}

/**
 * Prisma `include` for the populate tree plus every component of the root and
 * of each populated relation. Returns undefined when nothing is included.
 */
export function toPrismaInclude(
  resource: ResourceDef,
  populate: PopulateTree,
): Record<string, unknown> | undefined {
  const inc = componentIncludes(resource);
  for (const [name, node] of populate) {
    const rel = resolveRelation(resource, name);
    if (!rel) throw new Error(`Unknown relation ${name} on ${resource.name}`);
    const childInc = toPrismaInclude(rel.target(), node.children);
    const spec: Record<string, unknown> = {};
    if (childInc) spec.include = childInc;
    if (rel.many) spec.orderBy = { id: 'asc' };
    inc[rel.field] = Object.keys(spec).length ? spec : true;
  }
  return Object.keys(inc).length ? inc : undefined;
}

export function toPrismaPaging(p: Pagination): { skip: number; take: number } {
  if (p.kind === 'page') return { skip: (p.page - 1) * p.pageSize, take: p.pageSize };
  return { skip: p.start, take: p.limit };
}
