const LOVELACE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelace: number | undefined) => {
  if (lovelace) {
    return Number.parseFloat((lovelace / LOVELACE).toFixed(DECIMALS));
  }
  return 0;
};

export const correctDRepDirectoryFormat = (lovelace: number | undefined) => {
  if (lovelace) {
    return Number((lovelace / LOVELACE).toFixed(0))?.toLocaleString("en-US");
  }

  return 0;
};
