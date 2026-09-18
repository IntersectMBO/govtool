/**
 * Runtime surface of an interfaces-only package.
 *
 * Almost everything here is erased at compile time; `npm run typecheck` is what
 * checks it (see ./conformance.ts). What remains at runtime is the entry-point
 * wiring and `pinning.toAnchor`, and both are worth a real assertion: a broken
 * `exports` map or a namespace that erased to nothing would otherwise only
 * surface in a consumer.
 *
 * Runs against ./../dist, so `npm run build` must come first — `npm test` does
 * both.
 */

import assert from 'node:assert/strict';
import { test } from 'node:test';

import * as pkg from '../dist/index.js';
import * as chainDataEntry from '../dist/chain-data/index.js';
import * as metadataEntry from '../dist/metadata/index.js';
import { toAnchor } from '../dist/pinning/index.js';

test('root entry point exposes the three components as namespaces', () => {
  for (const component of ['chainData', 'metadata', 'pinning']) {
    assert.equal(
      typeof pkg[component],
      'object',
      `expected root export "${component}" to be a namespace object`,
    );
  }
});

test('component entry points resolve', () => {
  assert.equal(typeof chainDataEntry, 'object');
  assert.equal(typeof metadataEntry, 'object');
  assert.equal(typeof toAnchor, 'function');
});

test('toAnchor projects a pin record onto the on-chain anchor pair', () => {
  const pin = {
    cid: 'bafybeigdyrzt',
    url: 'ipfs://bafybeigdyrzt',
    gatewayUrls: ['https://ipfs.io/ipfs/bafybeigdyrzt'],
    dataHash: 'deadbeef',
    byteSize: 512,
    contentType: 'application/ld+json',
    status: 'pinned',
    pinnedAt: '2026-09-18T00:00:00Z',
    replicas: [],
  };

  const anchor = toAnchor(pin);

  // Exactly the anchor pair, and nothing else: a gateway url on chain would
  // pin the anchor to one gateway's availability.
  assert.deepEqual(anchor, {
    url: 'ipfs://bafybeigdyrzt',
    dataHash: 'deadbeef',
  });
  assert.deepEqual(Object.keys(anchor).sort(), ['dataHash', 'url']);
});

test('toAnchor is reachable through the root namespace too', () => {
  assert.equal(pkg.pinning.toAnchor, toAnchor);
});
