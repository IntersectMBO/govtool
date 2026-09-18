import { ChainDataError } from '@govtool/data-providers/chain-data';

/**
 * Awaits a call expected to fail and returns the `ChainDataError` it threw.
 *
 * Fails the test if the promise resolves, or rejects with anything else —
 * "every provider throws `ChainDataError` and nothing else across the
 * contract boundary" is a rule worth asserting rather than assuming.
 */
export async function expectChainDataError(
  promise: Promise<unknown>,
): Promise<ChainDataError> {
  try {
    await promise;
  } catch (error) {
    if (ChainDataError.is(error)) {
      return error;
    }
    throw new Error(`Expected a ChainDataError, got ${String(error)}`);
  }
  throw new Error('Expected the call to reject, but it resolved');
}
