/**
 * Byte spans of a transaction's CBOR, found without decoding it.
 *
 * CSL decodes recursively in wasm. A value nested a few thousand levels deep
 * (metadata under any label, a witness datum, an inline datum inside a tag-24
 * byte string) overflows its stack, and from then on every CSL call in the
 * process fails until it restarts. Such a transaction costs one fee and the
 * survey route is public, so the survey path never hands CSL a whole
 * transaction: this reader walks the CBOR heads iteratively to find the body
 * and the auxiliary data, and only the auxiliary data, once its nesting is
 * bounded, is decoded.
 *
 * Input is untrusted: every length is checked against the buffer, and
 * anything unexpected throws a CborError.
 */

export class CborError extends Error {}

const fail = (why: string): never => {
  throw new CborError(why);
};

/** Deepest nesting of arrays, maps and tags handed to CSL; CIP-179 surveys need a handful. */
export const MAX_DECODED_DEPTH = 64;

interface Head {
  major: number;
  info: number;
  /** The argument; for info 31 (indefinite length or break) it is 31. */
  arg: number;
  next: number;
}

function head(buf: Buffer, pos: number): Head {
  if (pos >= buf.length) fail('truncated');
  const initial = buf[pos]!;
  const major = initial >> 5;
  const info = initial & 0x1f;
  let arg = info;
  let next = pos + 1;
  if (info >= 24 && info <= 27) {
    const size = 1 << (info - 24);
    if (next + size > buf.length) fail('truncated');
    // Past 2^53 an argument is imprecise, but it is then only ever a length, and too long.
    arg = size === 8 ? Number(buf.readBigUInt64BE(next)) : buf.readUIntBE(next, size);
    next += size;
  } else if (info >= 28 && info <= 30) {
    fail(`reserved additional info ${info}`);
  }
  return { major, info, arg, next };
}

/** One item: where it ends, and its deepest nesting of arrays, maps and tags. */
export interface CborItem {
  start: number;
  end: number;
  depth: number;
}

/** The item at `start`, walked with an explicit stack so no nesting overflows the JS stack either. */
export function itemAt(buf: Buffer, start: number): CborItem {
  let pos = start;
  let depth = 0;
  // Items still to read in each open array, map or tag; Infinity until its break.
  const open: number[] = [];
  const completed = () => {
    while (open.length > 0) {
      const top = open.length - 1;
      if (open[top] === Infinity) return;
      open[top] = open[top]! - 1;
      if (open[top]! > 0) return;
      open.pop();
    }
  };
  do {
    if (open.length > 0 && open[open.length - 1] === Infinity && buf[pos] === 0xff) {
      pos += 1;
      open.pop();
      completed();
      continue;
    }
    const h = head(buf, pos);
    pos = h.next;
    const indefinite = h.info === 31;
    switch (h.major) {
      case 0:
      case 1:
        if (indefinite) fail('indefinite integer');
        break;
      case 2:
      case 3:
        if (!indefinite) {
          if (h.arg > buf.length - pos) fail('length exceeds input');
          pos += h.arg;
          break;
        }
        // Definite chunks of the same major type, then a break.
        for (;;) {
          if (pos >= buf.length) fail('truncated');
          if (buf[pos] === 0xff) {
            pos += 1;
            break;
          }
          const chunk = head(buf, pos);
          if (chunk.major !== h.major || chunk.info === 31) fail('bad indefinite-length chunk');
          if (chunk.arg > buf.length - chunk.next) fail('length exceeds input');
          pos = chunk.next + chunk.arg;
        }
        break;
      case 4:
      case 5:
      case 6: {
        if (indefinite && h.major === 6) fail('indefinite tag');
        const items = indefinite ? Infinity : h.major === 4 ? h.arg : h.major === 5 ? h.arg * 2 : 1;
        // Every item takes at least one byte.
        if (items !== Infinity && items > buf.length - pos) fail('length exceeds input');
        if (items > 0) {
          open.push(items);
          depth = Math.max(depth, open.length);
          continue;
        }
        break;
      }
      default:
        // Simple values and floats: the head already took their bytes.
        if (indefinite) fail('unexpected break');
    }
    completed();
  } while (open.length > 0);
  return { start, end: pos, depth };
}

export interface TransactionSpans {
  body: Buffer;
  /** `null` when the transaction carries none. */
  auxiliaryData: { bytes: Buffer; depth: number } | null;
}

/**
 * A Shelley-to-Mary transaction is `[body, witnesses, auxiliary data]`, a later
 * one `[body, witnesses, is_valid, auxiliary data]`, where absent auxiliary
 * data is `null`. The whole buffer must be that one array.
 */
export function transactionSpans(buf: Buffer): TransactionSpans {
  const h = head(buf, 0);
  if (h.major !== 4) fail('not a transaction array');
  const indefinite = h.info === 31;
  const elements: CborItem[] = [];
  let pos = h.next;
  while (indefinite ? buf[pos] !== 0xff : elements.length < h.arg) {
    if (elements.length === 4) fail('a transaction array of more than 4 items');
    const item = itemAt(buf, pos);
    elements.push(item);
    pos = item.end;
  }
  if (indefinite) pos += 1;
  if (pos !== buf.length) fail('trailing bytes');
  if (elements.length !== 3 && elements.length !== 4) fail(`a transaction array of ${elements.length} items`);

  const body = elements[0]!;
  if (buf[body.start]! >> 5 !== 5) fail('transaction body is not a map');
  if (elements.length === 4 && buf[elements[2]!.start] !== 0xf4 && buf[elements[2]!.start] !== 0xf5) {
    fail('is_valid is not a boolean');
  }
  const aux = elements[elements.length - 1]!;
  return {
    body: buf.subarray(body.start, body.end),
    auxiliaryData: buf[aux.start] === 0xf6 ? null : { bytes: buf.subarray(aux.start, aux.end), depth: aux.depth },
  };
}

/** The auxiliary data hash a transaction body declares (key 7), hex; `null` when it declares none. */
export function auxiliaryDataHash(body: Buffer): string | null {
  const h = head(body, 0);
  if (h.major !== 5) fail('transaction body is not a map');
  const indefinite = h.info === 31;
  let hash: string | null = null;
  let pos = h.next;
  for (let i = 0; indefinite ? body[pos] !== 0xff : i < h.arg; i++) {
    const key = head(body, pos);
    const value = itemAt(body, itemAt(body, pos).end);
    if (key.major === 0 && key.arg === 7) {
      const v = head(body, value.start);
      if (v.major !== 2 || v.info === 31 || v.arg !== 32 || v.next + 32 !== value.end) fail('auxiliary data hash is not 32 bytes');
      if (hash !== null) fail('two auxiliary data hashes');
      hash = body.subarray(v.next, value.end).toString('hex');
    }
    pos = value.end;
  }
  return hash;
}
