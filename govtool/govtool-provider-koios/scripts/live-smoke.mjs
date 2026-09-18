/**
 * Live smoke test against a real Koios deployment.
 *
 * Not part of `npm run verify`: it hits the network, its assertions are
 * "did this route answer at all, and with what", and mainnet data moves.
 * It exists to catch the one thing the unit tests structurally cannot — that
 * the row shapes in `src/rows` still match what Koios actually serves.
 *
 *   npm run build && npm run smoke
 *   KOIOS_NETWORK=preprod KOIOS_TOKEN=... npm run smoke
 */
import { createKoiosProvider } from '../dist/index.js';

const p = createKoiosProvider({
  network: process.env.KOIOS_NETWORK ?? 'mainnet',
  token: process.env.KOIOS_TOKEN,
  timeoutMs: 45000,
  maxRetries: 1,
});
const results = [];
async function check(name, fn) {
  try {
    const value = await fn();
    results.push(['OK  ', name, value]);
  } catch (e) {
    results.push(['FAIL', name, `${e.code ?? ''} ${e.message}`]);
  }
}

await check('network.getNetworkInfo', async () => {
  const { data } = await p.network.getNetworkInfo();
  return `${data.network} magic=${data.networkMagic} era=${data.era} epoch=${data.epoch.no} block=${data.tip.block}`;
});
await check('network.getProtocolParams', async () => {
  const { data } = await p.network.getProtocolParams();
  return `epoch=${data.epoch} govDeposit=${data.govActionDeposit} drepActivity=${data.drepActivity} dvt=${data.dvt}`;
});
await check('network.getStakeDistribution', async () => {
  const { data } = await p.network.getStakeDistribution();
  return `dreps=${data.totalStakeControlledByDReps} abstain=${data.alwaysAbstainVotingPower} spos=${data.totalStakeControlledBySPOs}`;
});
await check('network.getTreasury', async () => {
  const { data } = await p.network.getTreasury();
  return `epoch=${data.epoch} balance=${data.balance} delta=${data.delta}`;
});
await check('network.listEpochs', async () => {
  const { data } = await p.network.listEpochs({ limit: 3 });
  return data.map(e => e.epoch).join(',');
});
await check('system.getHealth', async () => {
  const { data } = await p.system.getHealth();
  return `${data[0].status} lag=${data[0].secondsSinceLastUpdate}s`;
});

let firstProposal;
await check('proposals.list', async () => {
  const { data } = await p.governance.proposals.list({ limit: 3 });
  firstProposal = data.elements[0];
  return `total=${data.total} first=${firstProposal?.type} status=${firstProposal?.lifecycle.status} title=${JSON.stringify(firstProposal?.metadata?.body?.title ?? null).slice(0,50)}`;
});
await check('proposals.get + typed body', async () => {
  const { data } = await p.governance.proposals.get(firstProposal.id);
  return `${data.type} body=${data.body?.type} meta=${data.metadata?.status}`;
});
await check('proposals.getTallies', async () => {
  const { data } = await p.governance.proposals.getTallies(firstProposal.id);
  const d = data.find(t => t.role === 'drep');
  return `drep yes=${d.stake.yes} counts=${JSON.stringify(d.count)} notVoted=${d.notVotedStake}`;
});
await check('proposals.listVotes', async () => {
  const { data } = await p.governance.proposals.listVotes(firstProposal.id, { limit: 3 });
  return `n=${data.elements.length} first=${data.elements[0]?.vote} tx=${data.elements[0]?.txRef.txHash?.slice(0,12)} role=${data.elements[0]?.voter.role}`;
});
await check('proposals.listActivity', async () => {
  const { data } = await p.governance.proposals.listActivity(firstProposal.id, { limit: 5 });
  return data.elements.map(e => e.type).join(',');
});
await check('proposals.getEnacted(UpdateCommittee)', async () => {
  const { data } = await p.governance.proposals.getEnacted('UpdateCommittee');
  return `${data?.action.id?.slice(0,20)} body=${data?.body?.type} added=${data?.body?.added?.length} quorum=${JSON.stringify(data?.body?.quorum)}`;
});

let firstDrep;
await check('dreps.list', async () => {
  // A retired DRep still carries a `votingPower` object with amount "0", so
  // picking on truthiness alone lands on a retired credential and leaves the
  // power-history and activity routes untested. Pick one with real power.
  const { data } = await p.governance.dreps.list({ limit: 25, expand: ['metadata'] });
  const withPower = data.elements.filter(
    (d) => d.votingPower && BigInt(d.votingPower.amount) > 0n,
  );
  firstDrep = withPower[0] ?? data.elements[0];
  return `total=${data.total} n=${data.elements.length} withPower=${withPower.length} first=${firstDrep?.id?.slice(0,20)} status=${firstDrep?.registration.status}`;
});
await check('dreps.get', async () => {
  const { data } = await p.governance.dreps.get(firstDrep.id, { expand: ['metadata','activity'] });
  return `power=${data.votingPower?.amount} status=${data.registration.status} votes=${data.activity?.votesCast} meta=${data.metadata?.status} name=${JSON.stringify(data.metadata?.body?.givenName ?? null)}`;
});
await check('dreps.getVotingPower history', async () => {
  const { data } = await p.governance.dreps.getVotingPower(firstDrep.id, { fromEpoch: 650 });
  return `points=${data.length} latest=${data[0]?.epoch}:${data[0]?.amount}`;
});
await check('dreps.listHistory', async () => {
  const { data } = await p.governance.dreps.listHistory(firstDrep.id, { limit: 3 });
  return data.elements.map(e => e.type).join(',');
});
await check('dreps.getVotingPowers(predefined)', async () => {
  const { data } = await p.governance.dreps.getVotingPowers(['drep_always_abstain','drep_always_no_confidence']);
  return data.map(e => `${e.subject.option}=${e.votingPower?.amount}`).join(' ');
});

await check('committee.getCommittee', async () => {
  const { data } = await p.governance.committee.getCommittee();
  return `members=${data.members.length} quorum=${data.quorum.numerator}/${data.quorum.denominator} resigned=${data.members.filter(m=>m.hasResigned).length}`;
});
await check('committee.getConstitution', async () => {
  const { data } = await p.governance.committee.getConstitution();
  return `url=${data.anchor.url?.slice(0,40)} enacted=${JSON.stringify(data.enactedAt)}`;
});

await check('votes.list', async () => {
  const { data } = await p.governance.votes.list({ limit: 3, role: ['spo'] });
  return `total=${data.total} first=${data.elements[0]?.vote} role=${data.elements[0]?.voter.role}`;
});
await check('pools.get', async () => {
  const { data } = await p.governance.pools.get('pool1m83drqwlugdt9jn7jkz8hx3pne53acfkd539d9cj8yr92dr4k9y');
  return `ticker=${data.ticker} power=${data.votingPower?.amount} live=${data.liveStake}`;
});

const STAKE = 'stake1u9ya9ajt7gmmdqlr503t2s72duvtmc4kwdck299568x4xjqqd22nl';
await check('accounts.get', async () => {
  const { data } = await p.accounts.get(STAKE, { expand: ['balance','votingPower','delegation','poolDelegation'] });
  return `reg=${data.isRegistered} power=${data.votingPower?.amount} total=${data.balance?.total} rest=${data.balance?.rewardsRest} drep=${data.delegation?.target.kind}`;
});
await check('accounts.listDelegationHistory', async () => {
  const { data } = await p.accounts.listDelegationHistory(STAKE, { limit: 3 });
  return `n=${data.elements.length} kinds=${data.elements.map(e=>e.kind).join(',')} to=${data.elements[0]?.to?.kind}`;
});
await check('accounts.listStakeEvents', async () => {
  const { data } = await p.accounts.listStakeEvents(STAKE, { limit: 3 });
  return data.elements.map(e => `${e.action}@${e.at.epoch}`).join(',');
});
await check('transactions.get', async () => {
  const { data } = await p.transactions.get('c9b11588e508c325a260754086a6c5f40fe0ba4daa1c92a77ee59c8b55949755');
  return `${data.status} conf=${data.confirmations} effects=${data.effects?.map(e=>e.kind).join(',')}`;
});
await check('metrics.getAvailable', async () => {
  const { data } = await p.governance.metrics.getAvailable();
  return `dreps=${data.totalRegisteredDReps} actions=${data.totalGovernanceActions} live=${data.totalLiveGovernanceActions} drepVotes=${data.totalDRepVotes} cc=${data.committee?.size}`;
});

for (const [status, name, value] of results) {
  console.log(`${status} ${name.padEnd(38)} ${value}`);
}
const failed = results.filter((r) => r[0] === 'FAIL').length;
console.log(`\n${results.length - failed}/${results.length} live routes OK`);
process.exit(failed === 0 ? 0 : 1);
