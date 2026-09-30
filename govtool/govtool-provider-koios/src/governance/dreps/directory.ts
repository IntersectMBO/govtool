/**
 * Reading the DRep directory out of Koios.
 *
 * Koios splits a DRep across endpoints, and each answers a different question:
 *
 *   /drep_list     every credential that ever registered, with a registered
 *                  flag. GET, pageable, filterable — the directory's spine.
 *   /drep_info     status, ledger expiry, deposit, current-epoch voting power,
 *                  live delegator count. POST by id, at most ~70 ids per body
 *                  (Koios answers 413 above 5,120 bytes).
 *   /drep_updates  every registration / update / retirement certificate, dated
 *                  by block time only. GET, bulk or filtered by id.
 *
 * Nothing here caches; each call reads what it needs.
 */
import type { Anchor, DRepStatus, RegistrationEvent } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../../context';
import { internal } from '../../errors';
import { chunkForFilter, inList } from '../../http';
import { decodeDRepId, encodeDRepId } from '../../ids';
import { stripBytea, toLovelace } from '../../numbers';
import type { DRepInfoRow, DRepListRow, DRepUpdateRow } from '../../rows';

export const PREDEFINED = ['drep_always_abstain', 'drep_always_no_confidence'] as const;

/** Above this many ids, reading every certificate in bulk is fewer requests than filtering. */
const BULK_UPDATES_THRESHOLD = 200;

export interface DirectoryEntry {
  id: string;
  hash: string;
  isScript: boolean;
  registered: boolean;
}

/** A Koios DRep id, checked and re-encoded canonically; Koios is trusted for data, not for form. */
export function canonicalDRepId(row: { drep_id: string; hex?: string | null; has_script: boolean }): string {
  if (row.hex) return encodeDRepId(stripBytea(row.hex), row.has_script);
  const { hash, isScript } = decodeDRepId(row.drep_id);
  return encodeDRepId(hash, isScript);
}

/** `/drep_list`, whole, optionally narrowed by PostgREST filters. */
export async function loadDirectory(ctx: Ctx, filter: Record<string, string> = {}): Promise<DirectoryEntry[]> {
  const rows = await ctx.http.getAll<DRepListRow>('drep_list', filter, { order: 'hex.asc,has_script.asc' });
  return rows.map((row) => ({
    id: canonicalDRepId(row),
    hash: stripBytea(row.hex),
    isScript: row.has_script,
    registered: row.registered === true,
  }));
}

export interface InfoSet {
  dreps: Map<string, DRepInfoRow>;
  predefined: Map<string, DRepInfoRow>;
}

/** `/drep_info` for the given ids, or for every REGISTERED DRep when `ids` is omitted. */
export async function loadDRepInfo(ctx: Ctx, opts: { ids?: readonly string[]; includePredefined?: boolean } = {}): Promise<InfoSet> {
  const ids = opts.ids ?? (await loadDirectory(ctx, { registered: 'eq.true' })).map((e) => e.id);
  const wanted = [...ids, ...(opts.includePredefined ? PREDEFINED : [])];
  const rows = wanted.length === 0 ? [] : await ctx.http.postChunked<DRepInfoRow>('drep_info', '_drep_ids', wanted);
  const dreps = new Map<string, DRepInfoRow>();
  const predefined = new Map<string, DRepInfoRow>();
  for (const row of rows) {
    if ((PREDEFINED as readonly string[]).includes(row.drep_id)) predefined.set(row.drep_id, row);
    else dreps.set(canonicalDRepId(row), row);
  }
  return { dreps, predefined };
}

/**
 * The ledger status. Retired from the registration state; otherwise read off
 * the expiry epoch Koios reports (`expires_epoch_no`, which it takes as the
 * later of the ledger's `active_until` and the last activity + drepActivity):
 * inactive once the current epoch is past it (SPEC.md §5.3). Koios' own
 * `active` flag is used only where it reports no expiry at all.
 */
export function drepStatusOf(row: DRepInfoRow, currentEpoch: number): DRepStatus {
  if (row.drep_status !== 'registered') return 'retired';
  if (row.expires_epoch_no === null || row.expires_epoch_no === undefined) return row.active ? 'active' : 'inactive';
  return currentEpoch > row.expires_epoch_no ? 'inactive' : 'active';
}

/** `/drep_updates` for the given DReps, grouped by canonical id. */
export async function loadUpdates(ctx: Ctx, ids: readonly string[] | 'all'): Promise<Map<string, DRepUpdateRow[]>> {
  const select = 'drep_id,hex,has_script,update_tx_hash,cert_index,block_time,action,deposit,meta_url,meta_hash';
  let rows: (DRepUpdateRow & { hex?: string | null; has_script: boolean })[];
  if (ids === 'all' || ids.length > BULK_UPDATES_THRESHOLD) {
    rows = await ctx.http.getAll('drep_updates', {}, { select, order: 'block_time.asc,cert_index.asc,update_tx_hash.asc' });
  } else if (ids.length === 0) {
    rows = [];
  } else {
    const parts = await Promise.all(
      chunkForFilter(ids).map((chunk) =>
        ctx.http.getAll<DRepUpdateRow & { hex?: string | null; has_script: boolean }>(
          'drep_updates',
          { drep_id: inList(chunk) },
          { select, order: 'block_time.asc,cert_index.asc,update_tx_hash.asc' },
        ),
      ),
    );
    rows = parts.flat();
  }
  const out = new Map<string, DRepUpdateRow[]>();
  const keep = ids === 'all' ? undefined : new Set(ids);
  for (const row of rows) {
    const id = canonicalDRepId(row);
    if (keep && !keep.has(id)) continue;
    const list = out.get(id);
    if (list) list.push(row);
    else out.set(id, [row]);
  }
  for (const list of out.values()) list.sort(certOrder);
  return out;
}

/** Oldest first: block time, then certificate index within the transaction. */
export const certOrder = (a: DRepUpdateRow, b: DRepUpdateRow) =>
  a.block_time - b.block_time || a.cert_index - b.cert_index || (a.update_tx_hash < b.update_tx_hash ? -1 : a.update_tx_hash > b.update_tx_hash ? 1 : 0);

export const toAnchor = (url: string | null | undefined, hash: string | null | undefined): Anchor | null =>
  url && hash ? { url, dataHash: stripBytea(hash) } : null;

/** What a DRep's certificates say, read the way the db-sync provider reads its tables. */
export interface CertFacts {
  /** Newest registration certificate. */
  latest: DRepUpdateRow;
  /** First registration ever: the start of the DRep's vote window. */
  first: DRepUpdateRow;
  /** Newest update certificate since the newest registration. */
  latestUpdate: DRepUpdateRow | null;
  /** Newest retirement, when it is newer than the newest registration. */
  retirement: DRepUpdateRow | null;
  /** The anchor in force: the newest registration or update certificate's. */
  anchor: Anchor | null;
}

export function certFacts(id: string, certs: readonly DRepUpdateRow[] | undefined): CertFacts {
  const list = certs ?? [];
  const regs = list.filter((c) => c.action === 'registered');
  const latest = regs[regs.length - 1];
  const first = regs[0];
  if (!latest || !first) throw internal('Koios has no registration certificate for a listed DRep', { id });
  const after = (c: DRepUpdateRow) => certOrder(c, latest) > 0;
  const updates = list.filter((c) => c.action === 'updated' && after(c));
  const retirements = list.filter((c) => c.action === 'deregistered' && after(c));
  const current = list.filter((c) => c.action !== 'deregistered');
  const cur = current[current.length - 1]!;
  return {
    latest,
    first,
    latestUpdate: updates[updates.length - 1] ?? null,
    retirement: retirements[retirements.length - 1] ?? null,
    anchor: toAnchor(cur.meta_url, cur.meta_hash),
  };
}

export async function toRegistrationEvent(ctx: Ctx, row: DRepUpdateRow): Promise<RegistrationEvent> {
  const at = await ctx.chain.stampAt(row.block_time);
  return {
    txRef: { txHash: row.update_tx_hash, index: row.cert_index },
    at,
    anchor: toAnchor(row.meta_url, row.meta_hash),
    // Only a registration carries a deposit; an update has none.
    deposit: row.action === 'registered' && row.deposit !== null ? toLovelace(row.deposit) : null,
  };
}
