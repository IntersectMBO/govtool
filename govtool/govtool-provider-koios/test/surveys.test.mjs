/**
 * Surveys area (CIP-179, SPEC.md §5.6) over a fake Koios `/tx_cbor`. Test
 * transactions are built here with CSL; the payloads are the real-shape
 * fixtures from frontend/src/cip179/fixtures/dbSyncMetadata.json, copied.
 * The answer is decoded by a small CBOR reader below, independent of CSL, so
 * a value CSL lost on the way out cannot also be lost on the way back in.
 * Runs against ./dist.
 */
import assert from 'node:assert/strict';
import { test } from 'node:test';

import CSL from '@emurgo/cardano-serialization-lib-nodejs';

import { MAX_TX_CBOR_BYTES } from '../dist/surveys.js';
import { provider, rejectsWith } from './helpers.mjs';

/* -- fixtures ---------------------------------------------------------------- */

/** The inner label-17 value: one survey definition. */
const PAYLOAD_ONE =
  '820081a80005018200581c22222222222222222222222222222222222222222222222222222222026541756469740373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201';
/** db-sync's row for it: the singleton map `{17: payload}`. */
const ROW_ONE = `a111${PAYLOAD_ONE}`;
/** db-sync's row for a transaction publishing two definitions. */
const ROW_BATCHED =
  'a111820082a80005018200581c22222222222222222222222222222222222222222222222222222222026541756469740373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201a80005018200581c2222222222222222222222222222222222222222222222222222222202665365636f6e640373496e646570656e64656e742066697874757265048100051901f4068100078184016643686f6f7365826141614201';
const PAYLOAD_BATCHED = ROW_BATCHED.slice(4);
/**
 * Integers at the metadata bounds and past 2^53: `{1: 2^64-1, 2: -(2^64-1),
 * 3: 2^53+1, 4: h'deadbeef', "t": "text"}`.
 */
const PAYLOAD_BIG = 'a5011bffffffffffffffff023bfffffffffffffffe031b00200000000000010444deadbeef61746474657874';

/* -- a transaction, built with CSL ------------------------------------------- */

/**
 * A minimal Conway-shaped transaction. `metadata` is `[label, metadatumHex][]`;
 * `aux: false` leaves auxiliary data out. `salt` varies the input so each
 * transaction has its own hash.
 */
function buildTx({ metadata = [], aux = true, salt = 1 } = {}) {
  const owned = [];
  const own = (o) => (owned.push(o), o);
  try {
    const inputs = own(CSL.TransactionInputs.new());
    inputs.add(own(CSL.TransactionInput.new(own(CSL.TransactionHash.from_hex(Buffer.alloc(32, salt).toString('hex'))), 0)));
    const body = own(CSL.TransactionBody.new_tx_body(inputs, own(CSL.TransactionOutputs.new()), own(CSL.BigNum.from_str('170000'))));
    let auxData;
    if (aux) {
      const md = own(CSL.GeneralTransactionMetadata.new());
      for (const [label, hex] of metadata) {
        const prev = md.insert(own(CSL.BigNum.from_str(String(label))), own(CSL.TransactionMetadatum.from_hex(hex)));
        if (prev) own(prev);
      }
      // Not owned: Transaction.new consumes it.
      auxData = CSL.AuxiliaryData.new();
      auxData.set_metadata(md);
    }
    const tx = own(CSL.Transaction.new(body, own(CSL.TransactionWitnessSet.new()), auxData));
    const cbor = tx.to_hex();
    const fixed = own(CSL.FixedTransaction.from_hex(cbor));
    return { cbor, hash: own(fixed.transaction_hash()).to_hex() };
  } finally {
    for (const o of owned) o.free();
  }
}

/** A fake `/tx_cbor` over `{ hash: cbor }`, answering only the hashes asked for. */
const txCbor = (byHash) => (_url, init) =>
  init.body._tx_hashes.filter((h) => Object.hasOwn(byHash, h)).map((h) => ({ tx_hash: h, cbor: byHash[h] }));

/* -- an independent CBOR reader ---------------------------------------------- */

/**
 * Enough of RFC 8949 for transaction metadata: integers as BigInt, byte
 * strings as Buffer, text, arrays and maps (as Map), definite or indefinite.
 * Rejects trailing bytes.
 */
function decodeCbor(hex) {
  const buf = Buffer.from(hex, 'hex');
  assert.equal(buf.toString('hex'), hex.toLowerCase(), 'hex input');
  let pos = 0;
  const arg = (info) => {
    if (info < 24) return BigInt(info);
    const n = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
    if (n === undefined) throw new Error(`unsupported additional info ${info}`);
    const v = BigInt(`0x${buf.subarray(pos, pos + n).toString('hex')}`);
    pos += n;
    return v;
  };
  const item = () => {
    if (pos >= buf.length) throw new Error('truncated');
    const ib = buf[pos++];
    const major = ib >> 5;
    const info = ib & 0x1f;
    const indefinite = info === 31 && major >= 2 && major <= 5;
    const len = indefinite ? undefined : arg(info);
    const until = (read) => {
      const out = [];
      if (indefinite) {
        while (buf[pos] !== 0xff) out.push(read());
        pos++;
      } else for (let i = 0n; i < len; i++) out.push(read());
      return out;
    };
    switch (major) {
      case 0:
        return len;
      case 1:
        return -1n - len;
      case 2: {
        if (indefinite) return Buffer.concat(until(item));
        const b = Buffer.from(buf.subarray(pos, pos + Number(len)));
        pos += Number(len);
        return b;
      }
      case 3: {
        if (indefinite) return until(item).join('');
        const s = buf.subarray(pos, pos + Number(len)).toString('utf8');
        pos += Number(len);
        return s;
      }
      case 4:
        return until(item);
      case 5:
        return new Map(until(() => [item(), item()]));
      default:
        throw new Error(`major type ${major} is not metadata`);
    }
  };
  const value = item();
  if (pos !== buf.length) throw new Error('trailing bytes');
  return value;
}

/* -- tests ------------------------------------------------------------------- */

test('one definition: the singleton {17: payload}, the same value db-sync stores', async () => {
  const tx = buildTx({ metadata: [[17, PAYLOAD_ONE]] });
  const { chainData, calls } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  const res = await chainData.surveys.getDefinition(tx.hash);
  assert.deepEqual(res.meta, { provider: 'koios', network: 'mainnet' });
  assert.deepEqual(Object.keys(res.data).sort(), ['metadataLabel', 'payloadCborHex', 'txHash']);
  assert.equal(res.data.txHash, tx.hash);
  assert.equal(res.data.metadataLabel, 17);
  assert.ok(res.data.payloadCborHex.startsWith('a111'), res.data.payloadCborHex);
  assert.deepEqual(decodeCbor(res.data.payloadCborHex), decodeCbor(ROW_ONE));
  // Not required by the contract, but CSL keeps this payload's encoding.
  assert.equal(res.data.payloadCborHex, ROW_ONE);

  assert.equal(calls.length, 1);
  assert.equal(calls[0].endpoint, 'tx_cbor');
  assert.equal(calls[0].method, 'POST');
  assert.deepEqual(calls[0].body, { _tx_hashes: [tx.hash] });
});

test('the hash is matched case-insensitively, trimmed, and reported lowercase', async () => {
  const tx = buildTx({ metadata: [[17, PAYLOAD_ONE]] });
  const { chainData, calls } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  const res = await chainData.surveys.getDefinition(`  ${tx.hash.toUpperCase()}\n`);
  assert.equal(res.data.txHash, tx.hash);
  assert.deepEqual(calls[0].body, { _tx_hashes: [tx.hash] });
  assert.deepEqual(decodeCbor(res.data.payloadCborHex), decodeCbor(ROW_ONE));
});

test('a malformed hash is INVALID_INPUT, rejected asynchronously, and never sent', async () => {
  const { chainData, calls } = provider({ tx_cbor: () => assert.fail('must not be called') });
  for (const bad of ['xyz', 'ab'.repeat(31) + 'a', 'ab'.repeat(33), 'g'.repeat(64), '', 42, null, undefined, { hash: 'ab'.repeat(32) }]) {
    await rejectsWith(() => chainData.surveys.getDefinition(bad), 'INVALID_INPUT');
  }
  assert.equal(calls.length, 0);
});

test('no such transaction is null', async () => {
  const { chainData } = provider({ tx_cbor: [] });
  const res = await chainData.surveys.getDefinition('ab'.repeat(32));
  assert.deepEqual(res, { data: null, meta: { provider: 'koios', network: 'mainnet' } });
});

test('a transaction without label 17 is null, other labels notwithstanding', async () => {
  const tx = buildTx({ metadata: [[674, 'a1636d736781657468657265']] });
  const { chainData } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  assert.equal((await chainData.surveys.getDefinition(tx.hash)).data, null);
});

test('a transaction without auxiliary data is null', async () => {
  const tx = buildTx({ aux: false });
  assert.ok(tx.cbor.endsWith('f5f6'), 'no auxiliary data');
  const { chainData } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  assert.equal((await chainData.surveys.getDefinition(tx.hash)).data, null);
});

test('only label 17 is served, as a singleton map', async () => {
  const tx = buildTx({ metadata: [[674, 'a1636d736781657468657265'], [17, PAYLOAD_ONE], [721, 'a0']] });
  const { chainData } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  const { data } = await chainData.surveys.getDefinition(tx.hash);
  const decoded = decodeCbor(data.payloadCborHex);
  assert.deepEqual([...decoded.keys()], [17n]);
  assert.deepEqual(decoded, decodeCbor(ROW_ONE));
});

test('batched definitions are preserved whole, not one selected index', async () => {
  const tx = buildTx({ metadata: [[17, PAYLOAD_BATCHED]] });
  const { chainData } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  const { data } = await chainData.surveys.getDefinition(tx.hash);
  const decoded = decodeCbor(data.payloadCborHex);
  assert.deepEqual(decoded, decodeCbor(ROW_BATCHED));
  const [version, definitions] = decoded.get(17n);
  assert.equal(version, 0n);
  assert.equal(definitions.length, 2);
  assert.deepEqual(
    definitions.map((d) => d.get(2n)),
    ['Audit', 'Second'],
  );
});

test('CBOR types survive: byte strings, text, integer keys, integers past 2^53 and at the 64-bit bounds', async () => {
  const tx = buildTx({ metadata: [[17, PAYLOAD_BIG]] });
  const { chainData } = provider({ tx_cbor: txCbor({ [tx.hash]: tx.cbor }) });
  const { data } = await chainData.surveys.getDefinition(tx.hash);
  const value = decodeCbor(data.payloadCborHex).get(17n);
  assert.ok(value instanceof Map);
  assert.equal(value.get(1n), 2n ** 64n - 1n);
  assert.equal(value.get(2n), -(2n ** 64n - 1n));
  assert.equal(value.get(3n), 2n ** 53n + 1n);
  assert.deepEqual(value.get(4n), Buffer.from('deadbeef', 'hex'));
  assert.equal(value.get('t'), 'text');
  // And the same value through CSL, the way a consumer may read it.
  const md = CSL.GeneralTransactionMetadata.from_hex(data.payloadCborHex);
  const key = CSL.BigNum.from_str('17');
  const got = md.get(key);
  try {
    assert.equal(md.len(), 1);
    assert.equal(got.to_hex(), PAYLOAD_BIG);
  } finally {
    got.free();
    key.free();
    md.free();
  }
});

test('missing CBOR (not retained by the instance) is PROVIDER_UNAVAILABLE, retryable, not null', async () => {
  const hash = 'cd'.repeat(32);
  for (const cbor of [null, '', '   ', undefined]) {
    const { chainData } = provider({ tx_cbor: [{ tx_hash: hash, ...(cbor === undefined ? {} : { cbor }) }] });
    const p = chainData.surveys.getDefinition(hash);
    await rejectsWith(p, 'PROVIDER_UNAVAILABLE');
    await p.catch((e) => assert.equal(e.retryable, true));
  }
});

test('malformed transaction CBOR is INTERNAL', async () => {
  const hash = 'cd'.repeat(32);
  const valid = buildTx({ metadata: [[17, PAYLOAD_ONE]] }).cbor;
  for (const cbor of ['zz', 'abc', 'deadbeef', '84a0', valid.slice(0, valid.length - 20), 42]) {
    const { chainData } = provider({ tx_cbor: [{ tx_hash: hash, cbor }] });
    await rejectsWith(chainData.surveys.getDefinition(hash), 'INTERNAL');
  }
});

test('transaction CBOR over the size bound is INTERNAL, before decoding', async () => {
  assert.equal(MAX_TX_CBOR_BYTES, 1024 * 1024);
  const hash = 'cd'.repeat(32);
  const { chainData } = provider({ tx_cbor: [{ tx_hash: hash, cbor: '84'.repeat(MAX_TX_CBOR_BYTES + 1) }] });
  const p = chainData.surveys.getDefinition(hash);
  await rejectsWith(p, 'INTERNAL');
  await p.catch((e) => assert.equal(e.details?.bytes, MAX_TX_CBOR_BYTES + 1, 'refused on size, not on a decode failure'));
  // At the bound it is decoded (and fails as CBOR, which is a different refusal).
  const atBound = provider({ tx_cbor: [{ tx_hash: hash, cbor: '84'.repeat(MAX_TX_CBOR_BYTES) }] });
  await assert.rejects(atBound.chainData.surveys.getDefinition(hash), (e) => e.code === 'INTERNAL' && e.details?.bytes === undefined);
});

test('CBOR of another transaction, or a row for another hash, is INTERNAL', async () => {
  const asked = buildTx({ metadata: [[17, PAYLOAD_ONE]], salt: 1 });
  const other = buildTx({ metadata: [[17, PAYLOAD_ONE]], salt: 2 });
  assert.notEqual(asked.hash, other.hash);

  const swapped = provider({ tx_cbor: [{ tx_hash: asked.hash, cbor: other.cbor }] });
  await rejectsWith(swapped.chainData.surveys.getDefinition(asked.hash), 'INTERNAL');

  const wrongRow = provider({ tx_cbor: [{ tx_hash: other.hash, cbor: other.cbor }] });
  await rejectsWith(wrongRow.chainData.surveys.getDefinition(asked.hash), 'INTERNAL');
});

test('transport failures propagate with their codes, never as null', async () => {
  const hash = 'ab'.repeat(32);
  const cases = [
    [{ status: 500, body: { message: 'boom' } }, 'PROVIDER_UNAVAILABLE'],
    [{ status: 429, body: {}, headers: { 'retry-after': '1' } }, 'PROVIDER_RATE_LIMITED'],
    [{ status: 401, body: {} }, 'PROVIDER_UNAVAILABLE'],
    [{ status: 400, body: {} }, 'INTERNAL'],
  ];
  for (const [answer, code] of cases) {
    const { chainData } = provider({ tx_cbor: () => answer });
    await rejectsWith(chainData.surveys.getDefinition(hash), code);
  }
  const { chainData } = provider({ tx_cbor: () => new Response('not json', { status: 200 }) });
  await rejectsWith(chainData.surveys.getDefinition(hash), 'INTERNAL');
});
