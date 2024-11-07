const LOVELACE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelace: number | undefined) => {
  if (lovelace) {
    return Number.parseFloat((lovelace / LOVELACE).toFixed(DECIMALS));
  }
  return 0;
};

export const correctVoteAdaFormat = (
  lovelace: number | undefined,
  locale: string | undefined = undefined,
) => {
  if (lovelace) {
    const ada = lovelace / LOVELACE;
    return ada.toLocaleString(locale, {
      minimumFractionDigits: 3,
      maximumFractionDigits: 3,
    });
  }
  return "0,000";
};

export const correctDRepDirectoryFormat = (lovelace: number | undefined) => {
  if (lovelace) {
    return Number((lovelace / LOVELACE).toFixed(0))?.toLocaleString("en-US");
  }

  return "0";
};
