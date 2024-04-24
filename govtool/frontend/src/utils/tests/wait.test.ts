import { wait } from "..";

describe("wait function", () => {
  it("resolves after the specified time", async () => {
    const startTime = Date.now();
    const waitTime = 2000;

    await wait(waitTime);

    const endTime = Date.now();
    const elapsedTime = endTime - startTime;

    expect(elapsedTime).toBeGreaterThanOrEqual(waitTime - 100);
    expect(elapsedTime).toBeLessThanOrEqual(waitTime + 100);
  });

  it("resolves after the default time if no time is specified", async () => {
    const startTime = Date.now();

    await wait();

    const endTime = Date.now();
    const elapsedTime = endTime - startTime;

    expect(elapsedTime).toBeGreaterThanOrEqual(4900);
    expect(elapsedTime).toBeLessThanOrEqual(5100);
  });
});
