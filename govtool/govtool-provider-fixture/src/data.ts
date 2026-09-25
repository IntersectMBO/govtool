/**
 * The frozen dataset.
 *
 * Everything in `data/*.json` is ALREADY in `@govtool/data-providers` shapes —
 * mapping happened at capture time (see scripts/capture.mjs). So this module
 * loads and nothing more, and a shape bug fails when the fixture is captured
 * rather than when it is read.
 */

import { readFileSync } from 'node:fs';
import { join } from 'node:path';

import type {
  Account,
  Committee,
  Constitution,
  DRep,
  DRepDelegator,
  Delegation,
  GenesisParams,
  GovAction,
  NetworkInfo,
  ProtocolParams,
  SpoVoter,
  StakeDistribution,
  Treasury,
  VoteRecord,
} from '@govtool/data-providers/chain-data';

/** A DRep plus the metadata document captured alongside it. */
export type FixtureDRep = DRep & { _metadataBody?: unknown };

export interface FixtureData {
  capturedAt: string;
  source: string;
  networkInfo: NetworkInfo;
  protocolParams: ProtocolParams;
  /** The Shelley genesis. Optional: a fixture captured before it was recorded has none. */
  genesisParams?: GenesisParams;
  stakeDistribution: StakeDistribution;
  treasury: Treasury;
  dreps: FixtureDRep[];
  drepDelegators: Record<string, DRepDelegator[]>;
  proposals: GovAction[];
  votes: Record<string, VoteRecord[]>;
  pools: SpoVoter[];
  committee: Committee | null;
  constitution: Constitution;
  accounts: { account: Account; delegation: Delegation | null }[];
}

export function loadFixture(path?: string): FixtureData {
  const file = path ?? join(__dirname, '..', 'data', 'mainnet.json');
  return JSON.parse(readFileSync(file, 'utf8')) as FixtureData;
}
