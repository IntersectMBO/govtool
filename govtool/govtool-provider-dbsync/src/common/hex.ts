import { invalidInput } from './errors';

const HEX = /^[0-9a-fA-F]+$/;

export function isHexText(value: string): boolean {
  return HEX.test(value) && value.length % 2 === 0;
}

/** Same rule as the legacy backend: hex digits only, even length. */
export function assertHexText(value: string): void {
  if (!isHexText(value)) {
    throw invalidInput('Not a valid hex value', { value });
  }
}
