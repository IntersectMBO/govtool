export async function waitedLoop(
  conditionFn,
  timeout = 60000,
  interval = 2000
) {
  const startTime = Date.now();
  while (Date.now() - startTime < timeout) {
    if (await conditionFn()) return true;
    await new Promise((resolve) => setTimeout(resolve, interval));
  }
  return false;
}
