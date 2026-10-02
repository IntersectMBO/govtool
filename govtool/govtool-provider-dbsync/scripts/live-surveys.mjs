#!/usr/bin/env node
/**
 * Live check of SurveysApi (CIP-179 definitions) against a real db-sync.
 *
 *   DBSYNC_POSTGRES_HOST=... DBSYNC_POSTGRES_PORT=... DBSYNC_POSTGRES_USER=...
 *   DBSYNC_POSTGRES_PASSWORD=... DBSYNC_DATABASE=... [NETWORK=preview] \
 *   SURVEY_TX_HASH=<a transaction carrying label-17 metadata> \
 *   node scripts/live-surveys.mjs
 *
 * Calls surveys.getDefinition(SURVEY_TX_HASH) and prints the result, then
 * checks it: a definition (not null), the hash reported lowercase, label 17,
 * a lowercase-hex payload starting a111 ({17: ...}), and an uppercase lookup
 * answering the same. It also checks that an all-zero hash answers null and
 * a malformed one rejects with INVALID_INPUT.
 *
 * Read-only. Build first (npm run build); it drives ./dist. Exits non-zero on
 * any failed check. Not part of npm test or npm run live.
 */
import { createDbSyncProvider } from '../dist/index.js';

const env = process.env;
for (const key of ['DBSYNC_POSTGRES_HOST', 'DBSYNC_POSTGRES_USER', 'DBSYNC_DATABASE', 'SURVEY_TX_HASH']) {
  if (!env[key]) {
    console.error(`missing ${key}`);
    process.exit(2);
  }
}
const network = env.NETWORK ?? 'preview';
const txHash = env.SURVEY_TX_HASH;
const connection = {
  host: env.DBSYNC_POSTGRES_HOST,
  port: Number(env.DBSYNC_POSTGRES_PORT ?? 5432),
  user: env.DBSYNC_POSTGRES_USER,
  password: env.DBSYNC_POSTGRES_PASSWORD,
  database: env.DBSYNC_DATABASE,
};

const { chainData, close } = createDbSyncProvider({ network, connection });

let failures = 0;
let passes = 0;
const check = (label, ok, detail) => {
  if (ok) passes++;
  else {
    failures++;
    console.log(`  FAIL ${label}${detail === undefined ? '' : ` — ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`}`);
  }
};
async function timed(label, fn) {
  const start = Date.now();
  try {
    return await fn();
  } finally {
    const ms = Date.now() - start;
    console.log(`  ${label}: ${ms} ms`);
    check(`${label} under 30 s`, ms < 30_000, `${ms} ms`);
  }
}

try {
  console.log(`surveys.getDefinition(${txHash}) on ${network}`);
  const result = await timed('getDefinition', () => chainData.surveys.getDefinition(txHash));
  console.log(JSON.stringify(result, null, 2));

  const d = result.data;
  check('a definition, not null', d !== null, 'null: the transaction is unknown or carries no label 17');
  if (d) {
    check('txHash is the input, lowercase', d.txHash === txHash.trim().toLowerCase(), d.txHash);
    check('metadataLabel is 17', d.metadataLabel === 17, d.metadataLabel);
    check('payloadCborHex is lowercase hex', /^([0-9a-f]{2})+$/.test(d.payloadCborHex));
    check('payloadCborHex is a singleton {17: ...} map', d.payloadCborHex.startsWith('a111'), d.payloadCborHex.slice(0, 8));
    console.log(`  payload: ${d.payloadCborHex.length / 2} bytes`);

    const upper = await timed('getDefinition (uppercase)', () => chainData.surveys.getDefinition(txHash.toUpperCase()));
    check('uppercase lookup answers the same', JSON.stringify(upper.data) === JSON.stringify(d));
  }

  const missing = await timed('getDefinition (unknown hash)', () => chainData.surveys.getDefinition('0'.repeat(64)));
  check('an unknown hash is null', missing.data === null, missing.data);

  try {
    const p = chainData.surveys.getDefinition('not-a-hash');
    check('a malformed hash returns a promise', p instanceof Promise);
    await p;
    check('a malformed hash -> INVALID_INPUT', false, 'resolved');
  } catch (error) {
    check('a malformed hash -> INVALID_INPUT', error?.code === 'INVALID_INPUT', `${error?.code}: ${error?.message}`);
  }
} catch (error) {
  check('no unexpected error', false, `${error?.code ?? ''} ${error?.message ?? error}`);
} finally {
  await close();
}

console.log(`\n${passes} passed, ${failures} failed`);
process.exit(failures === 0 ? 0 : 1);
