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

export async function functionWaitedAssert(
  fn,
  options: { timeout?: number; interval?: number; message?: string } = {
    timeout: 6000,
    interval: 2000,
    message: null,
  }
) {
  const { timeout, interval, message } = options;
  const startTime = Date.now();

  while (true) {
    try {
      await fn();
      return true;
    } catch (error) {
      if (Date.now() - startTime >= timeout) {
        const errorMessage = message || error.message;
        throw new Error(errorMessage);
      }
      await new Promise((resolve) => setTimeout(resolve, interval));
    }
  }
}
