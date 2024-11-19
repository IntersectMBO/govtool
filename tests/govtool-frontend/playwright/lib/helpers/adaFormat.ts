const LOVELACE = 1000000;

export const correctVoteAdaFormat = (
  lovelace: number | undefined,
  locale: string | undefined = undefined
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
