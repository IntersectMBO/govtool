export function lovelaceToAda(lovelace: number) {
  if (lovelace === 0) return 0;

  return lovelace / 1e6;
}
