// Resource descriptors: the wire names of SPEC §5 mapped to Prisma fields.
// One descriptor per model that appears on the wire. The query parser, the
// Prisma translator and the serializer all read them, so a wire name is
// declared exactly once.

/**
 * - `int`: integer column (counters, ids).
 * - `legacyId`: integer FK serialized as a decimal string (§3.2); filter
 *   values are coerced to integers.
 * - `float`, `string`, `boolean`, `json`: as named.
 * - `datetime`: ISO 8601 with milliseconds on the wire.
 * - `date`: `YYYY-MM-DD` on the wire.
 */
export type ScalarType = 'int' | 'legacyId' | 'float' | 'string' | 'boolean' | 'datetime' | 'date' | 'json';

export interface ScalarDef {
  /** Prisma field name. */
  field: string;
  type: ScalarType;
  /** Column is nullable (affects `$null` translation). Default true. */
  nullable?: boolean;
}

export interface RelationDef {
  /** Prisma relation field name. */
  field: string;
  target: () => ResourceDef;
  many: boolean;
  /**
   * Prisma FK scalar on this model for a to-one relation. Filters on
   * `<relation>.id` then compare the FK directly (cheaper, and `$null` means
   * "no related row").
   */
  fk?: string;
  /** The FK may be null. Default true. */
  nullable?: boolean;
}

export interface ComponentDef {
  /** Prisma relation field holding the component rows. */
  field: string;
  /** Wire name to Prisma field, serialized as plain values next to `id`. */
  scalars: Record<string, ScalarDef>;
}

export interface ResourceDef {
  name: string;
  /** Public scalars: serialized in `attributes`, candidates for filter/sort/fields. */
  scalars: Record<string, ScalarDef>;
  /**
   * Resolvable for filters when an allowlist names them, never serialized,
   * sorted on or selectable with `fields` (e.g. `comments_reports.hash`).
   */
  hidden?: Record<string, ScalarDef>;
  relations?: Record<string, RelationDef>;
  /** Repeatable components, always serialized inline (§3.3). */
  components?: Record<string, ComponentDef>;
  /** `createdAt`/`updatedAt` in attributes. Default true. */
  timestamps?: boolean;
  /** `publishedAt` in attributes (draft-and-publish types in Strapi). */
  publishedAt?: boolean;
  /** Names accepted in `fields` that yield nothing (Δ2: `username` on users). */
  noopFields?: readonly string[];
}

const ID: ScalarDef = { field: 'id', type: 'int', nullable: false };
const CREATED: ScalarDef = { field: 'createdAt', type: 'datetime', nullable: false };
const UPDATED: ScalarDef = { field: 'updatedAt', type: 'datetime', nullable: false };
const PUBLISHED: ScalarDef = { field: 'publishedAt', type: 'datetime', nullable: false };

/** Scalars the serializer emits (no `id`), in declaration order. */
export function attributeScalars(r: ResourceDef): Array<[string, ScalarDef]> {
  const out: Array<[string, ScalarDef]> = Object.entries(r.scalars);
  if (r.timestamps !== false) out.push(['createdAt', CREATED], ['updatedAt', UPDATED]);
  if (r.publishedAt) out.push(['publishedAt', PUBLISHED]);
  return out;
}

/** Any scalar reachable by wire name, including `id` and hidden ones. */
export function resolveScalar(r: ResourceDef, name: string): ScalarDef | undefined {
  if (name === 'id') return ID;
  if (Object.prototype.hasOwnProperty.call(r.scalars, name)) return r.scalars[name];
  if (r.hidden && Object.prototype.hasOwnProperty.call(r.hidden, name)) return r.hidden[name];
  if (r.timestamps !== false && name === 'createdAt') return CREATED;
  if (r.timestamps !== false && name === 'updatedAt') return UPDATED;
  if (r.publishedAt && name === 'publishedAt') return PUBLISHED;
  return undefined;
}

export function resolveRelation(r: ResourceDef, name: string): RelationDef | undefined {
  if (r.relations && Object.prototype.hasOwnProperty.call(r.relations, name)) {
    return r.relations[name];
  }
  return undefined;
}

/**
 * Wire names of the public scalars plus `id`, `createdAt`, `updatedAt` (and
 * `publishedAt` where the type has it): the default filterable and sortable
 * set of §4.2. JSON columns are left out. Optionally prefixed with a
 * relation path.
 */
export function scalarPaths(r: ResourceDef, prefix?: string): string[] {
  const names = [
    'id',
    ...attributeScalars(r)
      .filter(([, d]) => d.type !== 'json')
      .map(([n]) => n),
  ];
  return prefix ? names.map((n) => `${prefix}.${n}`) : names;
}

export function defineResource(def: ResourceDef): ResourceDef {
  return def;
}

/** Shorthands for descriptor columns. `nullable` defaults as the column does. */
export const col = {
  int: (field: string, nullable = false): ScalarDef => ({ field, type: 'int', nullable }),
  legacyId: (field: string, nullable = false): ScalarDef => ({ field, type: 'legacyId', nullable }),
  float: (field: string, nullable = true): ScalarDef => ({ field, type: 'float', nullable }),
  str: (field: string, nullable = true): ScalarDef => ({ field, type: 'string', nullable }),
  bool: (field: string, nullable = false): ScalarDef => ({ field, type: 'boolean', nullable }),
  datetime: (field: string, nullable = true): ScalarDef => ({ field, type: 'datetime', nullable }),
  date: (field: string, nullable = true): ScalarDef => ({ field, type: 'date', nullable }),
  json: (field: string, nullable = false): ScalarDef => ({ field, type: 'json', nullable }),
};
