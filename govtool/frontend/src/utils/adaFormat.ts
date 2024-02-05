const LOVELACE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelace: number | undefined) => {
  return lovelace
    ? Number.parseFloat((lovelace / LOVELACE).toFixed(DECIMALS))
    : 0;
};
