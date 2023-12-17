/* eslint-disable indent */
/* eslint-disable @typescript-eslint/no-explicit-any */
export const callAll =
  (...fns: any[]) =>
  (...args: any[]) =>
    fns.forEach((fn) => fn && fn(...args));
