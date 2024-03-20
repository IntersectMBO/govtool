export const callAll = (...fns: any[]) => (...args: any[]) => fns.forEach((fn) => fn && fn(...args));
