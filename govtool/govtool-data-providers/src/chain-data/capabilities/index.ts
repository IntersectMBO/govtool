/**
 * Chain Data API — provider capability declaration and its derivation.
 *
 * Layering, innermost first:
 *
 *   axes.ts         the vocabulary: cost, absence, option support, caveats.
 *   datasets.ts     `RouteId` derived from `ChainDataApiV1`, and the closed
 *                   dataset registry a provider declares into.
 *   fields.ts       field-level support, derived from the entity types.
 *   declaration.ts  what a provider declares, and the runtime dual of it.
 *   features.ts     the pure derivation from a declaration to what a UI reads.
 *
 * Nothing above depends on a framework, a provider or a consumer.
 */

export * from './axes';
export * from './datasets';
export * from './fields';
export * from './declaration';
export * from './features';
