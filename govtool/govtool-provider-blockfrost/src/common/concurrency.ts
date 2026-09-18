/**
 * Runs `worker` over `items` with a bounded number in flight.
 *
 * This provider has to hydrate entities one HTTP request at a time — the
 * directory endpoints return ids only — so a page of ten DReps is twenty
 * requests. Doing them serially makes a page read unusably slow; doing them
 * all at once invites a 429. The default of 8 is deliberately conservative
 * for a shared deployment.
 */
export async function mapWithConcurrency<T, R>(
  items: readonly T[],
  limit: number,
  worker: (item: T, index: number) => Promise<R>,
): Promise<R[]> {
  const results = new Array<R>(items.length);
  let next = 0;

  const runners = Array.from(
    { length: Math.max(1, Math.min(limit, items.length)) },
    async () => {
      for (;;) {
        const index = next++;
        if (index >= items.length) return;
        results[index] = await worker(items[index] as T, index);
      }
    },
  );

  await Promise.all(runners);
  return results;
}
