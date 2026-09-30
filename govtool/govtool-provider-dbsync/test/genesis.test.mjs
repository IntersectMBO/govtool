/**
 * getGenesisParams from a Shelley genesis file, checked against meta.start_time.
 * Runs against ./dist (npm run build first).
 */
import assert from 'node:assert/strict';
import { mkdtempSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { after, test } from 'node:test';

import { createDbSyncProvider } from '../dist/index.js';
import { META_START_SQL, mapShelleyGenesis } from '../dist/network/genesis.js';

const dir = mkdtempSync(join(tmpdir(), 'dbsync-genesis-'));
after(() => rmSync(dir, { recursive: true, force: true }));

// A devnet genesis: sub-second slots, 300-slot epochs, a supply above 2^53.
const DEVNET = `{
  "activeSlotsCoeff": 1.0,
  "epochLength": 300,
  "maxKESEvolutions": 60,
  "maxLovelaceSupply": 45000000000000001,
  "networkId": "Testnet",
  "networkMagic": 42,
  "securityParam": 10,
  "slotLength": 0.2,
  "slotsPerKESPeriod": 129600,
  "systemStart": "2026-09-27T10:00:00Z",
  "updateQuorum": 1
}`;

function writeGenesis(name, text) {
  const path = join(dir, name);
  writeFileSync(path, text);
  return path;
}

function provider(startTime, shelleyGenesisPath) {
  const calls = [];
  const db = {
    async query(sql, params = []) {
      calls.push(sql);
      if (sql === META_START_SQL) return startTime === undefined ? [] : [{ start_time: startTime }];
      throw new Error(`unexpected SQL: ${sql.slice(0, 80)}`);
    },
  };
  return { calls, chainData: createDbSyncProvider({ network: 'devnet', db, shelleyGenesisPath }).chainData };
}

test('mapShelleyGenesis keeps a sub-second slot length and an exact supply', () => {
  assert.deepEqual(mapShelleyGenesis(DEVNET), {
    networkMagic: 42,
    networkId: 'Testnet',
    systemStart: '2026-09-27T10:00:00.000Z',
    epochLength: 300,
    slotLength: 0.2,
    activeSlotsCoefficient: { numerator: 1, denominator: 1 },
    securityParam: 10,
    slotsPerKesPeriod: 129600,
    maxKesEvolutions: 60,
    updateQuorum: 1,
    maxLovelaceSupply: '45000000000000001',
  });
});

test('mapShelleyGenesis refuses a file missing a constant rather than guessing', () => {
  const noSlot = DEVNET.replace('"slotLength": 0.2,', '');
  assert.throws(() => mapShelleyGenesis(noSlot), (e) => e.code === 'INTERNAL');
  assert.throws(() => mapShelleyGenesis('not json'), (e) => e.code === 'INTERNAL');
  assert.throws(() => mapShelleyGenesis(DEVNET.replace('"Testnet"', '"Devnet"')), (e) => e.code === 'INTERNAL');
});

test('getGenesisParams is absent without a genesis path', () => {
  const { chainData } = provider(new Date('2026-09-27T10:00:00Z'));
  assert.equal(chainData.network.getGenesisParams, undefined);
});

test('getGenesisParams serves the file when it matches the database start', async () => {
  const path = writeGenesis('match.json', DEVNET);
  const { chainData, calls } = provider(new Date('2026-09-27T10:00:00Z'), path);
  const { data, meta } = await chainData.network.getGenesisParams();
  assert.equal(data.systemStart, '2026-09-27T10:00:00.000Z');
  assert.equal(data.epochLength * data.slotLength, 60);
  assert.equal(meta.network, 'devnet');
  assert.deepEqual(calls, [META_START_SQL]);
});

test('getGenesisParams refuses a stale file from an earlier devnet run', async () => {
  const path = writeGenesis('stale.json', DEVNET);
  const { chainData } = provider(new Date('2026-09-27T11:30:00Z'), path);
  await assert.rejects(chainData.network.getGenesisParams(), (e) => e.code === 'INTERNAL');
});

test('getGenesisParams: an unreadable path is INTERNAL and does not leak the path', async () => {
  const { chainData } = provider(new Date('2026-09-27T10:00:00Z'), join(dir, 'missing.json'));
  await assert.rejects(chainData.network.getGenesisParams(), (e) => {
    assert.equal(e.code, 'INTERNAL');
    assert.ok(!e.message.includes(dir));
    return true;
  });
});
