/**
 * Chain Data API — what a provider declares.
 *
 * Two mechanisms answer two different questions (SPEC.md §4):
 *
 *   Is this available?            THE INTERFACE. A present method or field
 *                                 means available; an absent one means not.
 *                                 There are no availability booleans here.
 *
 *   Which option values work?     THIS DOCUMENT.
 *
 * Every entry below is an ARRAY OF SUPPORTED VALUES. A value absent from the
 * array is not supported, and `[]` is a complete refusal of that control.
 */

import type { DRepSort } from './governance/dreps';
import type { GovActionSort } from './governance/proposals';
import type { VoteSort } from './governance/votes';

/* ------------------------------------------------------------------------- */
/* Controls                                                                    */
/* ------------------------------------------------------------------------- */

/**
 * Kinds of input a DRep search will match.
 *
 * The caller does NOT name a mode — it passes what the user typed and the
 * provider applies whatever it supports. This declaration exists so a consumer
 * can tell the user what is searchable: the difference between a box labelled
 * "Search by DRep ID" and one labelled "Search by name or ID".
 */
export type SearchMode = 'exactId' | 'substring' | 'freeText' | 'adaHandle';

/** Filters a DRep listing may accept. */
export type DRepFilter = 'status' | 'kind';

/** Filters a governance action listing may accept. */
export type ProposalFilter = 'type' | 'status';

/**
 * How a vote aggregate can be expressed. `stake` is preferred — it is what the
 * ledger decides by for DReps and pools. At least one must be declared; an
 * empty list is not a legal declaration.
 */
export type VoteAggregateRepresentation = 'stake' | 'count' | 'percent';

/**
 * Optional ARGUMENTS a provider honours. Argument acceptance is not
 * structurally visible from the interface — a method's existence cannot say
 * whether it accepts an `epoch` — so it is declared here.
 */
export type OptionalArgument =
  /** `getProtocolParams({ epoch })` — parameters at a past epoch. */
  | 'protocolParams.epoch'
  /** A voter id on a PROPOSAL LISTING, to annotate or filter a whole page.
   *  The single-action form is required and is not declared. */
  | 'proposals.voterContextOnList';

/* ------------------------------------------------------------------------- */
/* The declaration                                                             */
/* ------------------------------------------------------------------------- */

export interface ProviderCapabilities {
  readonly sorts: {
    /** May be empty — a provider offering no DRep ordering is conformant. */
    readonly dreps: readonly DRepSort[];
    /** `newest` and `oldest` are required, so this is never empty. */
    readonly proposals: readonly GovActionSort[];
    readonly votes?: readonly VoteSort[];
  };
  readonly filters: {
    readonly dreps: readonly DRepFilter[];
    readonly proposals: readonly ProposalFilter[];
  };
  /** `exactId` is always present. */
  readonly search: readonly SearchMode[];
  readonly voteAggregate: readonly VoteAggregateRepresentation[];
  readonly optionalArguments: readonly OptionalArgument[];
}

/* ------------------------------------------------------------------------- */
/* Reading a declaration                                                       */
/* ------------------------------------------------------------------------- */

/** Narrow a UI's own option list to what the provider honours, in UI order. */
export function allowed<V extends string>(
  supported: readonly V[] | undefined,
  universe: readonly V[],
): readonly V[] {
  if (supported === undefined) return [];
  return universe.filter((value) => supported.includes(value));
}

export function honours(
  capabilities: ProviderCapabilities,
  argument: OptionalArgument,
): boolean {
  return capabilities.optionalArguments.includes(argument);
}
