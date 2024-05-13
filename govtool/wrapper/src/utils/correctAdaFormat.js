const LOVELACE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelace) => {
  if (lovelace) {
    return Number.parseFloat((lovelace / LOVELACE).toFixed(DECIMALS));
  }
  return 0;
};
