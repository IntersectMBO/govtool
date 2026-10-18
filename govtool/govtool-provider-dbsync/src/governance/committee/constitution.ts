/**
 * The current constitution (SPEC.md §5.5, D71).
 *
 * The opposite of the committee: each enacted NewConstitution replaces the
 * previous one outright and carries the anchor, so the enacted head of the
 * constitution lineage decides everything. Before any is enacted, the genesis
 * constitution (the `constitution` row with no proposal) is in force, and it
 * has no enacting action.
 *
 * db-sync writes a `constitution` row for every NewConstitution PROPOSAL, so a
 * row existing says nothing; only `enacted_epoch` does.
 */
import type { Constitution } from '@govtool/data-providers/chain-data';

import type { Ctx } from '../../context';
import { internal } from '../../errors';
import { encodeGovActionId } from '../../ids';
import { toIso } from '../../numbers';
import { orderLineage } from './lineage';

export interface ConstitutionRow {
  /** Null for the genesis constitution. */
  id: string | null;
  prev_id: string | null;
  tx_hash: string | null;
  index: number | null;
  enacted_epoch: number | null;
  epoch_start: Date | string | null;
  url: string;
  data_hash: string;
  script_hash: string | null;
}

export const CONSTITUTION_SQL = `
  SELECT g.id::text AS id, g.prev_gov_action_proposal::text AS prev_id, encode(t.hash, 'hex') AS tx_hash,
         g.index::int AS index, g.enacted_epoch::int AS enacted_epoch, e.start_time AS epoch_start,
         va.url, encode(va.data_hash, 'hex') AS data_hash, encode(c.script_hash, 'hex') AS script_hash
    FROM constitution c
    JOIN voting_anchor va ON va.id = c.voting_anchor_id
    LEFT JOIN gov_action_proposal g ON g.id = c.gov_action_proposal_id
    LEFT JOIN tx t ON t.id = g.tx_id
    LEFT JOIN epoch e ON e.no = g.enacted_epoch
   WHERE c.gov_action_proposal_id IS NULL
      OR (g.type = 'NewConstitution' AND g.enacted_epoch IS NOT NULL)`;

/** Pure assembly, exported for tests. */
export function assembleConstitution(rows: readonly ConstitutionRow[]): Constitution {
  const enacted = orderLineage(
    'constitution',
    rows.filter((row): row is ConstitutionRow & { id: string } => row.id !== null).map((row) => ({ ...row, prevId: row.prev_id })),
  );
  const head = enacted[enacted.length - 1];
  if (head) {
    return {
      anchor: { url: head.url, dataHash: head.data_hash },
      guardrailsScriptHash: head.script_hash,
      enactedBy: { id: encodeGovActionId(head.tx_hash!, head.index!), txHash: head.tx_hash!, index: head.index! },
      enactedAt:
        head.enacted_epoch === null
          ? null
          : { epoch: head.enacted_epoch, ...(head.epoch_start ? { time: toIso(head.epoch_start) } : {}) },
    };
  }
  const genesis = rows.filter((row) => row.id === null);
  if (genesis.length !== 1) {
    throw internal(`db-sync records ${genesis.length} genesis constitutions; expected exactly one`);
  }
  const g = genesis[0]!;
  return {
    anchor: { url: g.url, dataHash: g.data_hash },
    guardrailsScriptHash: g.script_hash,
    enactedBy: null,
    enactedAt: null,
  };
}

export async function readConstitution(ctx: Ctx): Promise<Constitution> {
  return assembleConstitution(await ctx.db.query<ConstitutionRow>(CONSTITUTION_SQL));
}
