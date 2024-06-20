export function getEnumKeyByValue(
  enumObj: any,
  value: string
): string | undefined {
  return Object.keys(enumObj).find((key) => enumObj[key] === value);
}
