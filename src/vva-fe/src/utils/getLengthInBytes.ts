export const getLengthInBytes = (str: string) => {
  const encoder = new TextEncoder();
  const utf8Bytes = encoder.encode(str);
  return utf8Bytes.length;
};
