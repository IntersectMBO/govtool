#!/usr/bin/env node
/** Read-only comparison: build the backend/providers, then run from the backend directory.
 * Reads an optional .env without printing credentials. Optional GOVTOOL_AGGREGATES_REPORT saves JSON.
 * Missing historical Koios SPO/CC aggregates are recorded separately from mismatched values.
 */
import { createRequire } from 'node:module';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { isDeepStrictEqual } from 'node:util';
const require = createRequire(import.meta.url);
const dotenv = require('dotenv');
// `.env` only supplies defaults; the environment alone is enough.
const env = {
  ...(existsSync('.env') ? dotenv.parse(readFileSync('.env')) : {}),
  ...process.env,
};
const { createDbSyncProvider } = require('@govtool/provider-dbsync');
const { createKoiosProvider } = require('@govtool/provider-koios');
const {
  toGovernanceActionDetailRow,
  relevantEpoch,
} = require('../dist/governance-actions/governance-actions.mapping.js');
const network = env.GOVTOOL_DBSYNC_NETWORK ?? 'preview';
if (network === 'devnet' && !env.GOVTOOL_KOIOS_BASE_URL) {
  throw new Error('A devnet comparison requires GOVTOOL_KOIOS_BASE_URL');
}
const db = createDbSyncProvider({
  network,
  connection: {
    host: env.GOVTOOL_DBSYNC_HOST,
    port: Number(env.GOVTOOL_DBSYNC_PORT ?? 5432),
    database: env.GOVTOOL_DBSYNC_DATABASE,
    user: env.GOVTOOL_DBSYNC_USER,
    password: env.GOVTOOL_DBSYNC_PASSWORD,
    connectionTimeoutMillis: 5000,
  },
  ...(env.GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH
    ? { shelleyGenesisPath: env.GOVTOOL_DBSYNC_SHELLEY_GENESIS_PATH }
    : {}),
});
const koios = createKoiosProvider({
  network: env.GOVTOOL_KOIOS_NETWORK ?? network,
  ...(env.GOVTOOL_KOIOS_BASE_URL
    ? { baseUrl: env.GOVTOOL_KOIOS_BASE_URL }
    : {}),
  ...(env.GOVTOOL_KOIOS_TOKEN ? { token: env.GOVTOOL_KOIOS_TOKEN } : {}),
  maxConcurrency: 2,
  maxRetries: 1,
  timeoutMs: 30000,
}).chainData;
const report = {
  checkedAt: new Date().toISOString(),
  network,
  tips: {},
  actions: [],
  errors: [],
};
let differences = 0;
const invariant = (aggregate) => {
  if (aggregate.representation === 'percent') return true;
  return (
    ['yes', 'no', 'abstain', 'notVoted', 'totalEligible'].every((k) =>
      /^\d+$/.test(aggregate[k]),
    ) &&
    BigInt(aggregate.yes) +
      BigInt(aggregate.no) +
      BigInt(aggregate.abstain) +
      BigInt(aggregate.notVoted) ===
      BigInt(aggregate.totalEligible)
  );
};
try {
  const [dTip, kTip] = await Promise.all([
    db.chainData.network.getNetworkInfo(),
    koios.network.getNetworkInfo(),
  ]);
  report.tips = {
    dbsync: dTip.data.currentEpoch,
    koios: kTip.data.currentEpoch,
  };
  if (dTip.meta.network !== kTip.meta.network)
    throw new Error('Provider networks differ');
  const samples = [];
  for (const status of ['live', 'enacted', 'expired']) {
    const page = await db.chainData.governance.proposals.list({
      status: [status],
      sort: 'newest',
      page: 1,
      size: 2,
    });
    samples.push(...page.data.elements);
  }
  for (const query of [
    { status: ['live'], type: ['InfoAction'] },
    { status: ['enacted'], type: ['HardForkInitiation'] },
  ]) {
    const page = await db.chainData.governance.proposals.list({
      ...query,
      sort: 'newest',
      page: 1,
      size: 1,
    });
    samples.push(
      ...page.data.elements.filter(
        (action) => !samples.some((a) => a.id === action.id),
      ),
    );
  }
  for (const sample of samples) {
    console.log(
      `Checking ${sample.lifecycle.status} ${sample.type} ${sample.id}`,
    );
    try {
      const [d, k] = await Promise.all([
        db.chainData.governance.proposals.get(sample.id),
        koios.governance.proposals.get(sample.id),
      ]);
      const detail = toGovernanceActionDetailRow(
        k.data,
        null,
        null,
        kTip.data.currentEpoch,
      );
      const entry = {
        id: sample.id,
        txHash: sample.txHash,
        index: sample.index,
        status: sample.lifecycle.status,
        type: sample.type,
        epoch: detail.used_epoch_no,
        dbsync: d.data.voteAggregates ?? [],
        koios: detail.vote_aggregates,
        governanceAction: detail,
        matchingRoles: [],
        unavailableRoles: [],
        differences: [],
      };
      if (
        !isDeepStrictEqual(detail.vote_aggregates, k.data.voteAggregates ?? [])
      )
        entry.differences.push('governanceActions projection changed aggregate');
      if (!detail.vote_aggregates.every(invariant))
        entry.differences.push('aggregate invariant failed');
      if (
        detail.used_epoch_no !== relevantEpoch(d.data, dTip.data.currentEpoch)
      )
        entry.differences.push('tally epochs differ');
      if (d.data.lifecycle.status !== k.data.lifecycle.status)
        entry.differences.push('lifecycle statuses differ');
      for (const a of d.data.voteAggregates ?? []) {
        const b = detail.vote_aggregates.find((value) => value.role === a.role);
        if (!b) {
          entry.unavailableRoles.push(a.role);
          if (a.role === 'drep' || sample.lifecycle.status === 'live') {
            entry.differences.push({
              role: a.role,
              fields: ['unexpectedly unavailable'],
            });
          }
          continue;
        }
        const fields = [
          'representation',
          'yes',
          'no',
          'abstain',
          'notVoted',
          'totalEligible',
          'passing',
        ];
        const changed = fields.filter((field) => a[field] !== b[field]);
        if (
          a.threshold.numerator * b.threshold.denominator !==
          b.threshold.numerator * a.threshold.denominator
        )
          changed.push('threshold');
        if (changed.length)
          entry.differences.push({ role: a.role, fields: changed });
        else entry.matchingRoles.push(a.role);
      }
      for (const a of detail.vote_aggregates) {
        if (!(d.data.voteAggregates ?? []).some((b) => b.role === a.role))
          entry.differences.push({
            role: a.role,
            fields: ['missing from dbsync'],
          });
      }
      differences += entry.differences.length;
      report.actions.push(entry);
      console.log(
        JSON.stringify({
          status: entry.status,
          matching: entry.matchingRoles,
          unavailable: entry.unavailableRoles,
          differences: entry.differences,
        }),
      );
    } catch (error) {
      report.errors.push({
        id: sample.id,
        code: error.code ?? 'ERROR',
        message: error.message,
      });
      console.log(`Read failed: ${error.code ?? 'ERROR'} ${error.message}`);
    }
  }
} catch (error) {
  report.errors.push({ code: error.code ?? 'ERROR', message: error.message });
} finally {
  await db.close();
  if (env.GOVTOOL_AGGREGATES_REPORT)
    writeFileSync(env.GOVTOOL_AGGREGATES_REPORT, JSON.stringify(report, null, 2));
}
console.log(
  JSON.stringify(
    { actions: report.actions.length, differences, errors: report.errors },
    null,
    2,
  ),
);
process.exitCode =
  differences || report.errors.length || report.actions.length === 0 ? 1 : 0;
