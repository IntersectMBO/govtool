/**
 * @govtool/data-providers — provider-agnostic contracts for GovTool's data layer.
 *
 * This package is interfaces and types, plus the two error classes every
 * implementation throws. It declares WHAT GovTool reads and writes; it contains
 * no implementation and no transport. An implementer is free to satisfy these
 * contracts from SQL, HTTP, a file, or a fixture, and consumers are written
 * against the contract rather than against any provider.
 *
 * Three components, deliberately separate (see the package README for why):
 *
 *   chain-data  Everything derivable from the ledger, through a swappable
 *               provider (db-sync, Koios, Blockfrost, Kupo).
 *   metadata    Off-chain document retrieval, hash verification, CIP
 *               validation and cache lifecycle.
 *   pinning     The author-side write path — pin user-authored metadata and
 *               return the anchor pair to submit on chain. Optional.
 *
 * Prefer the subpath imports; they are collision-free and make the component
 * boundary visible at the import site:
 *
 *   import type { DRep, ChainDataApiV1 } from '@govtool/data-providers/chain-data';
 *   import type { MetadataProjection } from '@govtool/data-providers/metadata';
 *   import type { PinRecord } from '@govtool/data-providers/pinning';
 *
 * The root entry point exposes the same three components as namespaces. It does
 * not flatten them: `chain-data` and `metadata` each define their own `Hex` and
 * `Timestamp` — by design, so `metadata` stands alone — and a flat re-export
 * would make those names ambiguous.
 *
 *   import type { chainData, metadata } from '@govtool/data-providers';
 *   declare const drep: chainData.DRep;
 */

export * as chainData from './chain-data';
export * as metadata from './metadata';
export * as pinning from './pinning';

/**
 * The three top-level service contracts and the two error classes,
 * re-exported flat because they are what an implementer declares `implements`
 * against and what a consumer catches, and they do not collide.
 */
export type { ChainDataApiV1 } from './chain-data';
export type { MetadataServiceV1 } from './metadata';
export type { PinningServiceV1 } from './pinning';
export { ChainDataError } from './chain-data';
export { PinningError } from './pinning';
