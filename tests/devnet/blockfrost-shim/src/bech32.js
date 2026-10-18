'use strict';

// Minimal bech32 (BIP-173) decoder. Cardano ids are plain bech32, never
// bech32m, and may be longer than BIP-173's 90 character limit.

const CHARSET = 'qpzry9x8gf2tvdw0s3jn54khce6mua7l';
const GENERATOR = [0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3];
const MAX_LENGTH = 1023;

function polymod(values) {
  let chk = 1;
  for (const v of values) {
    const top = chk >>> 25;
    chk = ((chk & 0x1ffffff) << 5) ^ v;
    for (let i = 0; i < 5; i++) {
      if ((top >>> i) & 1) chk ^= GENERATOR[i];
    }
  }
  return chk >>> 0;
}

function hrpExpand(hrp) {
  const out = [];
  for (let i = 0; i < hrp.length; i++) out.push(hrp.charCodeAt(i) >>> 5);
  out.push(0);
  for (let i = 0; i < hrp.length; i++) out.push(hrp.charCodeAt(i) & 31);
  return out;
}

function fromWords(words) {
  let acc = 0;
  let bits = 0;
  const out = [];
  for (const w of words) {
    acc = (acc << 5) | w;
    bits += 5;
    while (bits >= 8) {
      bits -= 8;
      out.push((acc >>> bits) & 0xff);
    }
  }
  // Leftover bits must be padding: fewer than 5 and all zero.
  if (bits >= 5 || ((acc << (8 - bits)) & 0xff) !== 0) return null;
  return Buffer.from(out);
}

/** `{ prefix, bytes }`, or `null` for anything that is not valid bech32. */
function decode(input) {
  if (typeof input !== 'string' || input.length < 8 || input.length > MAX_LENGTH) {
    return null;
  }
  const lower = input.toLowerCase();
  if (lower !== input && input.toUpperCase() !== input) return null;
  const sep = lower.lastIndexOf('1');
  if (sep < 1 || sep + 7 > lower.length) return null;
  const prefix = lower.slice(0, sep);
  const data = [];
  for (const c of lower.slice(sep + 1)) {
    const v = CHARSET.indexOf(c);
    if (v === -1) return null;
    data.push(v);
  }
  if (polymod(hrpExpand(prefix).concat(data)) !== 1) return null;
  const bytes = fromWords(data.slice(0, -6));
  return bytes ? { prefix, bytes } : null;
}

function toWords(bytes) {
  let acc = 0;
  let bits = 0;
  const out = [];
  for (const b of bytes) {
    acc = (acc << 8) | b;
    bits += 8;
    while (bits >= 5) {
      bits -= 5;
      out.push((acc >>> bits) & 31);
    }
  }
  if (bits > 0) out.push((acc << (5 - bits)) & 31);
  return out;
}

/** Encodes bytes under a prefix; used by the tests and for responses. */
function encode(prefix, bytes) {
  const words = toWords(bytes);
  const values = hrpExpand(prefix).concat(words, [0, 0, 0, 0, 0, 0]);
  const mod = polymod(values) ^ 1;
  const checksum = [];
  for (let i = 0; i < 6; i++) checksum.push((mod >>> (5 * (5 - i))) & 31);
  return prefix + '1' + words.concat(checksum).map((w) => CHARSET[w]).join('');
}

module.exports = { decode, encode };
