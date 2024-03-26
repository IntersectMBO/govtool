export const URL_REGEX =
  /^(?!.*\s)(ipfs:\/\/[a-zA-Z0-9]+|https?:\/\/(?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|[^/\s]+?\.[a-zA-Z]{2,})[^\s]*)/;
export const HASH_REGEX = /^[0-9A-Fa-f]+$/;
export const EMAIL_REGEX = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
export const NICKNAME_REGEX = /^\S+$/;

export function isValidURLFormat(str: string) {
  if (!str.length) return true;
  return URL_REGEX.test(str);
}

export function isValidHashFormat(str: string) {
  if (!str.length) return true;
  return HASH_REGEX.test(str);
}
