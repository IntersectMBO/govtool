/**
 * `GovernanceIndexV1` over the frozen dataset — an in-memory inverted index.
 *
 * This is the whole reason search is a separate component: the searchable text
 * lives in RESOLVED metadata documents, which chain data never holds. The
 * fixture captured those documents alongside, so it can index them.
 */

import type { PagedEnvelope } from '@govtool/data-providers/chain-data';
import type {
  DRepSearchApi,
  GovernanceIndexV1,
  PoolSearchApi,
  ProposalSearchApi,
} from '@govtool/data-providers/index-provider';

import type { FixtureData } from './data';
import { pagedEnvelope } from './paging';

/** Lowercased word set, so a term matches on any whole or partial word. */
function tokens(value: unknown, into: Set<string>): void {
  if (typeof value === 'string') {
    for (const word of value.toLowerCase().split(/[^a-z0-9]+/i)) {
      if (word) into.add(word);
    }
  } else if (Array.isArray(value)) {
    for (const v of value) tokens(v, into);
  } else if (value && typeof value === 'object') {
    for (const v of Object.values(value)) tokens(v, into);
  }
}

function buildIndex<T>(rows: readonly T[], text: (row: T) => unknown[]): Map<T, Set<string>> {
  const index = new Map<T, Set<string>>();
  for (const row of rows) {
    const set = new Set<string>();
    for (const value of text(row)) tokens(value, set);
    index.set(row, set);
  }
  return index;
}

function matches(index: Map<unknown, Set<string>>, row: unknown, term: string): boolean {
  const words = index.get(row);
  if (!words) return false;
  const needle = term.toLowerCase().trim();
  if (!needle) return true;
  for (const word of words) {
    if (word.includes(needle)) return true;
  }
  return false;
}

export function createIndex(data: FixtureData): GovernanceIndexV1 {
  const net = data.networkInfo.network;

  // DRep text is its id plus its RESOLVED CIP-119 document — the bit chain data
  // does not have.
  const drepIndex = buildIndex(data.dreps, (d) => [d.id, d._metadataBody]);
  const proposalIndex = buildIndex(data.proposals, (p) => [p.id, p.type, p.body]);
  const poolIndex = buildIndex(data.pools, (p) => [p.poolId]);

  const dreps: DRepSearchApi = {
    searchDReps: (q) => {
      let rows = data.dreps.filter((d) => matches(drepIndex, d, q.term));
      if (q.status?.length) rows = rows.filter((d) => q.status!.includes(d.status));
      if (q.kind?.length) rows = rows.filter((d) => q.kind!.includes(d.kind));
      return Promise.resolve(pagedEnvelope(rows, q, net) as PagedEnvelope<(typeof rows)[number]>);
    },
  };

  const proposals: ProposalSearchApi = {
    searchProposals: (q) => {
      let rows = data.proposals.filter((p) => matches(proposalIndex, p, q.term));
      if (q.type?.length) rows = rows.filter((p) => q.type!.includes(p.type));
      if (q.status?.length) rows = rows.filter((p) => q.status!.includes(p.lifecycle.status));
      return Promise.resolve(pagedEnvelope(rows, q, net));
    },
  };

  const pools: PoolSearchApi = {
    searchPools: (q) =>
      Promise.resolve(
        pagedEnvelope(data.pools.filter((p) => matches(poolIndex, p, q.term)), q, net),
      ),
  };

  return { dreps, proposals, pools };
}
