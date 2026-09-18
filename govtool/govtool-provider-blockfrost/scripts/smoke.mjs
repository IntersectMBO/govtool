/**
 * Live smoke test against a real Blockfrost-compatible deployment.
 *
 * Deliberately NOT part of `npm run verify`: it needs the network and its
 * results depend on chain state. Run it after changing a mapper, because the
 * unit tests answer from fixtures and cannot catch a field this deployment
 * spells differently.
 *
 *   BLOCKFROST_URL=https://mainnet.blockfrost.sireto.io npm run smoke
 */
import { createRequire } from 'node:module';

const require = createRequire(import.meta.url);
const { createBlockfrostProvider } = require('../dist/index.js');

const baseUrl = process.env.BLOCKFROST_URL ?? 'https://mainnet.blockfrost.sireto.io';
const projectId = process.env.BLOCKFROST_PROJECT_ID;

const provider = createBlockfrostProvider({
  baseUrl,
  ...(projectId ? { projectId } : {}),
  timeoutMs: 60_000,
  stalenessThresholdSeconds: 900,
});

let ok = 0;
let gaps = 0;
let failed = 0;

async function check(name, fn, { expectGap = false } = {}) {
  const started = Date.now();
  try {
    const value = await fn();
    const ms = Date.now() - started;
    if (expectGap) {
      console.log(`✗ ${name} — expected a declared gap but it answered (${ms}ms)`);
      failed += 1;
      return value;
    }
    console.log(`✓ ${name} (${ms}ms)`);
    ok += 1;
    return value;
  } catch (error) {
    const ms = Date.now() - started;
    if (expectGap && error?.code === 'CAPABILITY_UNSUPPORTED') {
      console.log(`· ${name} — declared gap: ${error.details?.reason ?? ''} (${ms}ms)`);
      gaps += 1;
      return null;
    }
    console.log(`✗ ${name} — ${error?.code ?? ''} ${error?.message ?? error} (${ms}ms)`);
    failed += 1;
    return null;
  }
}

console.log(`smoke: ${baseUrl}\n`);

const health = await check('system.getHealth', async () =>
  (await provider.system.getHealth()).data[0],
);
if (health) console.log(`    tip epoch ${health.tip?.epoch}, ${health.secondsSinceLastUpdate}s behind`);

await check('system.getCapabilities', () => provider.system.getCapabilities());
await check('network.getNetworkInfo', () => provider.network.getNetworkInfo());
await check('network.getProtocolParams', () => provider.network.getProtocolParams());
await check('network.getStakeDistribution', () => provider.network.getStakeDistribution());
await check('network.listEpochs', () => provider.network.listEpochs({ limit: 2 }));
await check('network.listBlocks', () => provider.network.listBlocks({ limit: 2 }));

const dreps = await check('governance.dreps.list', async () =>
  (await provider.governance.dreps.list({ limit: 2 })).data,
);
const drepId = dreps?.elements?.[0]?.id;
if (drepId) {
  await check('governance.dreps.get', () => provider.governance.dreps.get(drepId));
  await check('governance.dreps.getVotingPower', () => provider.governance.dreps.getVotingPower(drepId));
  await check('governance.dreps.getVotingPowers', () => provider.governance.dreps.getVotingPowers([drepId]));
  await check('governance.dreps.listDelegators', () => provider.governance.dreps.listDelegators(drepId, { limit: 2 }));
  await check('governance.dreps.listHistory', () => provider.governance.dreps.listHistory(drepId, { limit: 2 }));
  await check('governance.dreps.listVotes', () => provider.governance.dreps.listVotes(drepId), { expectGap: true });
}

const proposals = await check('governance.proposals.list', async () =>
  (await provider.governance.proposals.list({ limit: 3 })).data,
);
const proposalId = proposals?.elements?.[0]?.id;
if (proposalId) {
  await check('governance.proposals.get', () => provider.governance.proposals.get(proposalId));
  await check('governance.proposals.listVotes', () => provider.governance.proposals.listVotes(proposalId, { limit: 5 }));
  await check('governance.proposals.getTallies', () => provider.governance.proposals.getTallies(proposalId));
  const { txHash } = proposals.elements[0];
  await check('governance.proposals.listByTx', () => provider.governance.proposals.listByTx(txHash));
}
await check('governance.proposals.getEnacted', () => provider.governance.proposals.getEnacted('HardForkInitiation'));

const stake = process.env.BLOCKFROST_STAKE_ADDRESS;
if (stake) {
  await check('accounts.get', () => provider.accounts.get(stake, { expand: ['balance', 'delegation', 'poolDelegation'] }));
  await check('accounts.getDelegation', () => provider.accounts.getDelegation(stake));
  await check('accounts.listStakeEvents', () => provider.accounts.listStakeEvents(stake, { limit: 2 }));
} else {
  console.log('· accounts.* skipped — set BLOCKFROST_STAKE_ADDRESS to include them');
}

console.log('\n--- declared gaps ---');
await check('transactions.get', () => provider.transactions.get('00'.repeat(32)), { expectGap: true });
await check('governance.metrics.get', () => provider.governance.metrics.get(), { expectGap: true });
await check('governance.committee.getCommittee', () => provider.governance.committee.getCommittee(), { expectGap: true });
await check('governance.votes.list', () => provider.governance.votes.list(), { expectGap: true });
await check('governance.pools.list', () => provider.governance.pools.list(), { expectGap: true });
await check('network.getTreasury', () => provider.network.getTreasury(), { expectGap: true });

console.log(`\n${ok} served, ${gaps} declared gaps, ${failed} failed`);
process.exit(failed === 0 ? 0 : 1);
