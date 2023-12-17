const LOVELANCE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelance: number | undefined) => {
  return lovelance
    ? Number.parseFloat((lovelance / LOVELANCE).toFixed(DECIMALS))
    : 0;
};
