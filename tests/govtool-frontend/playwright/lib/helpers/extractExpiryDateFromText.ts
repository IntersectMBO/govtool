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

export default function extractExpiryDateFromText(text: string): Date | null {
  const regex = /(\d{1,2})th ([\w]{3}) (\d{4})/;
  const match = text.match(regex);

  if (match) {
    const day = parseInt(match[1]);
    const month = match[2];
    const year = parseInt(match[3]);

    return new Date(year, monthNames.indexOf(month), day);
  } else {
    return null;
  }
}
