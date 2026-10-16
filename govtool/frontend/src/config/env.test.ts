/* eslint-disable no-underscore-dangle */

afterEach(() => {
  delete window.__ENV__;
  vi.unstubAllEnvs();
  vi.resetModules();
});

it.each([
  [undefined, "true"],
  ["", "true"],
  ["$VITE_IS_CIP179_ENABLED", "true"],
  ["true", "true"],
  [true, true],
  ["false", "false"],
  [false, false],
])("resolves the CIP-179 runtime flag %s to %s", async (value, expected) => {
  vi.resetModules();
  vi.stubEnv("VITE_IS_CIP179_ENABLED", undefined);
  window.__ENV__ = { VITE_IS_CIP179_ENABLED: value };
  const { env } = await import("./env");
  expect(env.VITE_IS_CIP179_ENABLED).toBe(expected);
});

it("preserves a build-time opt-out when no runtime flag is set", async () => {
  vi.resetModules();
  vi.stubEnv("VITE_IS_CIP179_ENABLED", "false");
  window.__ENV__ = {};
  const { env } = await import("./env");
  expect(env.VITE_IS_CIP179_ENABLED).toBe("false");
});
