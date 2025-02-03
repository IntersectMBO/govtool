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
      maximumFractionDigits: 3,
    });
  }
  return "0";
};

export const correctDRepDirectoryFormat = (lovelace: number | undefined) => {
  if (lovelace) {
    return Number((lovelace / LOVELACE).toFixed(0))?.toLocaleString("en-US");
  }

  return "0";
};

export const correctAdaFormatWithSuffix = (
  lovelace: number | undefined,
  precision = 2,
) => {
  if (!lovelace) return "0";
  const ada = lovelace / LOVELACE;
  if (ada < 1000)
    return ada.toLocaleString("en-us", {
      maximumFractionDigits: precision,
    });

  const suffixes = ["k", "M", "B", "T"];
  const divisors = [1000, 1000000, 1000000000, 1000000000000];

  for (let i = 0; i < suffixes.length; i++) {
    if (ada < divisors[i] * 1000) {
      return (ada / divisors[i]).toFixed(precision) + suffixes[i];
    }
  }
};
