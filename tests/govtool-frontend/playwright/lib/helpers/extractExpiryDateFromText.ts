const monthNames = [
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec",
];

export default function extractExpiryDateFromText(
  text: string,
  isOutcome = false
): Date | null {
  const regex = isOutcome
    ? /(\d{1,2}) (\w{3}) (\d{4})/
    : /(\d{1,2})(st|nd|rd|th) ([\w]{3}) (\d{4})/;
  const match = text.match(regex);

  if (match) {
    const day = parseInt(match[1]);
    const month = isOutcome ? match[2] : match[3];
    const year = parseInt(isOutcome ? match[3] : match[4]);

    return new Date(year, monthNames.indexOf(month), day);
  } else {
    return null;
  }
}
