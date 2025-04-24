const LOVELACE = 1000000;

export const correctVoteAdaFormat = (
  lovelace: number | undefined,
  precision = 2
) => {
  if (lovelace) {
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
  }
  return "0";
};

export const correctDelegatedVoteAdaFormat = (ada: number | undefined) => {
  if (ada) {
    return ada.toLocaleString("en-us", {
      maximumFractionDigits: 3,
    });
  }
  return "0";
};

export const correctDRepDirectoryFormat = (ada: number | undefined) => {
  if (ada) {
    return Number(ada.toFixed(0))?.toLocaleString("en-US");
  }
  return "0";
};

export function formatWithThousandSeparator(
  value: number | string,
  isAda = true
): string {
  if (value === undefined || value === null) {
    return "0";
  }

  let numericValue: number;
  if (typeof value === "string") {
    numericValue = parseInt(value.replace(/,/g, ""));
  } else {
    numericValue = value;
  }

  if (!isAda) {
    numericValue = Math.ceil(numericValue / LOVELACE);
  }

  return numericValue.toLocaleString("en-US", {
    maximumFractionDigits: 3,
  });
}
