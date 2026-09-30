/**
 * Runtime surface of an interfaces-only package.
 *
 * Almost everything is erased at compile time; `npm run typecheck` over
 * ./conformance.ts is what checks the shapes. What survives to runtime is the
 * entry-point wiring and three small helpers — and both are worth a real
 * assertion, because a broken `exports` map or a namespace that erased to
 * nothing would otherwise only surface in a consumer.
 *
 * Runs against ../dist, so `npm run build` must come first — `npm test` does both.
 */

import assert from 'node:assert/strict';
import { test } from 'node:test';

import * as pkg from '../dist/index.js';
import * as chainData from '../dist/chain-data/index.js';
import { ChainDataError, allowed, honours, ratioEquals } from '../dist/chain-data/index.js';
import { PinningError } from '../dist/pinning/index.js';
import { METADATA_FAILURE_CATEGORY } from '../dist/metadata/index.js';

/* -- entry points ----------------------------------------------------------- */

test('the root entry exposes all six components as namespaces', () => {
  for (const name of [
    'chainData',
    'metadata',
    'pinning',
    'governanceIndex',
    'committeeInfo',
    'txMonitor',
  ]) {
    assert.equal(typeof pkg[name], 'object', `${name} namespace is missing`);
  }
});

test('the chain-data subpath resolves independently of the root', () => {
  assert.equal(typeof chainData.ChainDataError, 'function');
});

/* -- ratios ----------------------------------------------------------------- */

test('ratios compare by value, not by shape', () => {
  // 67/100 and 134/200 are the same threshold written two ways. Normalization
  // is not required of a provider, so structural equality would be wrong.
  assert.equal(ratioEquals({ numerator: 67, denominator: 100 }, { numerator: 134, denominator: 200 }), true);
  assert.equal(ratioEquals({ numerator: 2, denominator: 3 }, { numerator: 1, denominator: 2 }), false);
});

/* -- capability declarations ------------------------------------------------ */

const UI_SORTS = ['votingPower', 'registrationDate', 'activity', 'random'];

test('a declared option set narrows to the intersection, in the UI order', () => {
  assert.deepEqual(allowed(['random', 'votingPower'], UI_SORTS), ['votingPower', 'random']);
});

test('an empty declaration is a complete refusal, not "unset"', () => {
  assert.deepEqual(allowed([], UI_SORTS), []);
});

test('an undeclared control offers nothing', () => {
  // Absence is declination: a provider that says nothing has claimed nothing.
  assert.deepEqual(allowed(undefined, UI_SORTS), []);
});

test('optional arguments are honoured only when declared', () => {
  const caps = {
    sorts: { dreps: [], proposals: ['newest', 'oldest'] },
    filters: { dreps: [], proposals: [] },
    search: ['exactId'],
    voteAggregate: ['stake'],
    optionalArguments: ['protocolParams.epoch'],
  };
  assert.equal(honours(caps, 'protocolParams.epoch'), true);
  assert.equal(honours(caps, 'proposals.voterContextOnList'), false);
});

/* -- errors ----------------------------------------------------------------- */

test('ChainDataError carries its code and is recognisable across a boundary', () => {
  const error = new ChainDataError('CAPABILITY_UNSUPPORTED', 'no statement for it', {
    details: { route: 'governance.pools.listVotes' },
  });
  assert.equal(error.code, 'CAPABILITY_UNSUPPORTED');
  assert.equal(error.retryable, false);
  assert.deepEqual(error.details, { route: 'governance.pools.listVotes' });
  assert.equal(ChainDataError.is(error), true);
  assert.equal(ChainDataError.is(new Error('plain')), false);
});

test('PinningError carries its reason', () => {
  const error = new PinningError('QUOTA_EXCEEDED', 'owner is over quota');
  assert.equal(error.reason, 'QUOTA_EXCEEDED');
  assert.equal(PinningError.is(error), true);
});

/* -- metadata failure categories (D115, D124) -------------------------------- */

test('every metadata failure code maps to its display category', () => {
  assert.deepEqual(METADATA_FAILURE_CATEGORY, {
    FETCH_ERROR: 'NETWORK',
    EXCEEDS_LIMIT: 'INVALID_CONTENT',
    JSON_PARSE_ERROR: 'INVALID_CONTENT',
    HASH_MISMATCH: 'INVALID_CONTENT',
    SCHEMA_INVALID: 'SCHEMA_INVALID',
  });
});
