/**
 * The optional satellite components, over the same frozen dataset.
 */

import { createHash } from 'node:crypto';

import type { MetadataResult, MetadataServiceV1 } from '@govtool/data-providers/metadata';
import type { PinningServiceV1 } from '@govtool/data-providers/pinning';
import { PinningError } from '@govtool/data-providers/pinning';
import type { CommitteeInfoProviderV1 } from '@govtool/data-providers/committee-info';
import type { TransactionMonitorV1 } from '@govtool/data-providers/tx-monitor';

import type { FixtureData } from './data';

/**
 * Metadata resolved from the documents captured alongside each DRep.
 *
 * Nothing is fetched. A hash the fixture does not hold returns `FETCH_ERROR`,
 * which is the honest answer offline — the document may well exist, this
 * instance simply cannot reach it.
 */
export function createMetadataService(data: FixtureData): MetadataServiceV1 {
  const byHash = new Map<string, unknown>();
  for (const drep of data.dreps) {
    if (drep.anchor && drep._metadataBody) byHash.set(drep.anchor.dataHash, drep._metadataBody);
  }

  const now = () => data.capturedAt;

  const lookup = (hash: string): MetadataResult => {
    const body = byHash.get(hash);
    if (body === undefined) {
      return {
        ok: false,
        code: 'FETCH_ERROR',
        category: 'NETWORK',
        message: 'offline fixture: no captured document for this hash',
        checkedAt: now(),
      };
    }
    return { ok: true, hash, body, fetchedAt: now() };
  };

  return {
    getMetadata: (hash) => Promise.resolve(lookup(hash)),
    getCipMetadata: <TBody>(cip: number, hash: string) => {
      const result = lookup(hash);
      if (!result.ok) return Promise.resolve(result as MetadataResult<TBody>);
      // CIP-119 requires a `givenName`; the fixture validates that much so the
      // SCHEMA_INVALID path is exercisable offline.
      const body = result.body as { body?: { givenName?: unknown } };
      if (cip === 119 && body?.body?.givenName === undefined) {
        return Promise.resolve({
          ok: false,
          code: 'SCHEMA_INVALID',
          category: 'SCHEMA_INVALID',
          message: 'body.givenName is required by CIP-119 and is absent',
          checkedAt: now(),
        } satisfies MetadataResult<TBody>);
      }
      return Promise.resolve(result as MetadataResult<TBody>);
    },
    // Nothing is fetched offline, so a retry never refetches and there are
    // no reports to show.
    refresh: (hash) => Promise.resolve({ refetched: false, result: lookup(hash) }),
    getReport: () => Promise.resolve(null),
    listReports: () => Promise.resolve([]),
  };
}

/** An in-memory pin store. Content-addressed, so it behaves like the real thing. */
export function createPinningService(): PinningServiceV1 & { pins: Map<string, { data: Uint8Array; owner: string }> } {
  const pins = new Map<string, { data: Uint8Array; owner: string }>();
  const cid = (data: Uint8Array) =>
    `bafy${createHash('sha256').update(data).digest('hex').slice(0, 52)}`;

  return {
    pins,
    pinData: (data, owner) => {
      const id = cid(data);
      pins.set(id, { data, owner });
      return Promise.resolve(id);
    },
    getDataCid: (data) => Promise.resolve(cid(data)),
    unpin: (id) => {
      pins.delete(id);
      return Promise.resolve();
    },
    fetch: (id) => {
      const pin = pins.get(id);
      if (!pin) return Promise.reject(new PinningError('BACKEND_ERROR', `no such pin: ${id}`));
      return Promise.resolve(pin.data);
    },
    getHealth: () => Promise.resolve({ status: 'healthy' as const }),
  };
}

/**
 * No curated committee identity exists offline, so every lookup returns `null`
 * — which is the normal answer for this component, not an error.
 */
export function createCommitteeInfo(
  records: Record<string, import('@govtool/data-providers/committee-info').CommitteeMemberInfo> = {},
): CommitteeInfoProviderV1 {
  return {
    getMemberInfo: (coldCredentialId) =>
      Promise.resolve(records[coldCredentialId] ?? null),
  };
}

/**
 * Reports mempool then confirmed on a timer, so a UI's post-submission flow can
 * be driven end to end with no chain.
 */
export function createTxMonitor(options: { stepMs?: number } = {}): TransactionMonitorV1 {
  const step = options.stepMs ?? 50;
  return {
    add: (txHash, callback) => {
      callback({ txHash, state: 'mempool' });
      let confirmations = 0;
      const timer = setInterval(() => {
        confirmations++;
        callback({
          txHash,
          state: 'confirmed',
          confirmations,
          explorerUrl: `https://cardanoscan.io/transaction/${txHash}`,
        });
        if (confirmations >= 5) clearInterval(timer);
      }, step);
      // Deliberately NOT unref'd: the interval clears itself after the fifth
      // confirmation, so it cannot hold a process open, and unref'ing would
      // let a short-lived caller exit before any update arrived.
    },
  };
}
