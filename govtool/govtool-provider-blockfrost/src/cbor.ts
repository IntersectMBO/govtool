/**
 * A minimal, bounded CBOR (RFC 8949) reader for Cardano transactions, and the
 * three things this provider reads out of one.
 *
 * Why decode a transaction at all: Blockfrost's vote and DRep-certificate rows
 * carry no anchor. A vote's rationale anchor and a DRep update's anchor exist
 * only in the transaction body, which `/txs/{hash}/cbor` serves. Without them
 * a `VoteRecord.anchor` would have to be `null`, which the contract reads as
 * "the voter attached none" — a wrong answer, not a missing one.
 *
 * Input is untrusted: every length is checked against the buffer before it is
 * used, nesting is capped, and anything unexpected throws.
 */
import { internal } from './errors';

export type Cbor = number | bigint | string | Buffer | boolean | null | undefined | Cbor[] | CborMap | CborTag;

export class CborMap {
  constructor(readonly entries: [Cbor, Cbor][]) {}
  get(key: number): Cbor | undefined {
    const hit = this.entries.find(([k]) => k === key || (typeof k === 'bigint' && k === BigInt(key)));
    return hit?.[1];
  }
}

export class CborTag {
  constructor(
    readonly tag: number | bigint,
    readonly value: Cbor,
  ) {}
}

const MAX_DEPTH = 64;
const fail = (why: string): never => {
  throw internal(`Blockfrost returned transaction CBOR this provider cannot read: ${why}`);
};

class Reader {
  private pos = 0;
  constructor(private readonly buf: Buffer) {}

  done(): boolean {
    return this.pos === this.buf.length;
  }

  private need(n: number): void {
    if (n < 0 || this.pos + n > this.buf.length) fail('truncated');
  }

  private byte(): number {
    this.need(1);
    return this.buf[this.pos++]!;
  }

  /** The argument of a head byte; `null` for an indefinite length. */
  private arg(info: number): number | bigint | null {
    if (info < 24) return info;
    if (info === 24) return this.byte();
    if (info === 25) {
      this.need(2);
      const v = this.buf.readUInt16BE(this.pos);
      this.pos += 2;
      return v;
    }
    if (info === 26) {
      this.need(4);
      const v = this.buf.readUInt32BE(this.pos);
      this.pos += 4;
      return v;
    }
    if (info === 27) {
      this.need(8);
      const v = this.buf.readBigUInt64BE(this.pos);
      this.pos += 8;
      return v <= BigInt(Number.MAX_SAFE_INTEGER) ? Number(v) : v;
    }
    if (info === 31) return null;
    return fail(`reserved additional info ${info}`);
  }

  private length(info: number): number | null {
    const n = this.arg(info);
    if (n === null) return null;
    if (typeof n === 'bigint' || n > this.buf.length) return fail('length exceeds input');
    return n;
  }

  private isBreak(): boolean {
    this.need(1);
    if (this.buf[this.pos] === 0xff) {
      this.pos++;
      return true;
    }
    return false;
  }

  private chunks(major: number, info: number): Buffer {
    const n = this.length(info);
    if (n !== null) {
      this.need(n);
      const out = this.buf.subarray(this.pos, this.pos + n);
      this.pos += n;
      return Buffer.from(out);
    }
    const parts: Buffer[] = [];
    while (!this.isBreak()) {
      const head = this.byte();
      if (head >> 5 !== major || (head & 0x1f) === 31) fail('bad indefinite-length chunk');
      parts.push(this.chunks(major, head & 0x1f));
    }
    return Buffer.concat(parts);
  }

  read(depth = 0): Cbor {
    if (depth > MAX_DEPTH) fail('nesting too deep');
    const head = this.byte();
    const major = head >> 5;
    const info = head & 0x1f;
    switch (major) {
      case 0: {
        const v = this.arg(info);
        return v === null ? fail('indefinite integer') : v;
      }
      case 1: {
        const v = this.arg(info);
        if (v === null) return fail('indefinite integer');
        return typeof v === 'bigint' ? -1n - v : -1 - v;
      }
      case 2:
        return this.chunks(2, info);
      case 3:
        return this.chunks(3, info).toString('utf8');
      case 4: {
        const n = this.length(info);
        const out: Cbor[] = [];
        if (n === null) while (!this.isBreak()) out.push(this.read(depth + 1));
        else for (let i = 0; i < n; i++) out.push(this.read(depth + 1));
        return out;
      }
      case 5: {
        const n = this.length(info);
        const out: [Cbor, Cbor][] = [];
        if (n === null) while (!this.isBreak()) out.push([this.read(depth + 1), this.read(depth + 1)]);
        else for (let i = 0; i < n; i++) out.push([this.read(depth + 1), this.read(depth + 1)]);
        return new CborMap(out);
      }
      case 6: {
        const tag = this.arg(info);
        if (tag === null) return fail('indefinite tag');
        return new CborTag(tag, this.read(depth + 1));
      }
      default: {
        if (info === 20) return false;
        if (info === 21) return true;
        if (info === 22) return null;
        if (info === 23) return undefined;
        if (info === 24) return this.byte();
        if (info === 25 || info === 26 || info === 27) {
          const bytes = info === 25 ? 2 : info === 26 ? 4 : 8;
          this.need(bytes);
          const v =
            bytes === 2 ? halfFloat(this.buf.readUInt16BE(this.pos)) : bytes === 4 ? this.buf.readFloatBE(this.pos) : this.buf.readDoubleBE(this.pos);
          this.pos += bytes;
          return v;
        }
        if (info < 20) return info;
        return fail(`unexpected simple value ${info}`);
      }
    }
  }
}

function halfFloat(h: number): number {
  const exp = (h >> 10) & 0x1f;
  const mant = h & 0x3ff;
  const v = exp === 0 ? mant * 2 ** -24 : exp === 31 ? (mant ? NaN : Infinity) : (mant + 1024) * 2 ** (exp - 25);
  return h & 0x8000 ? -v : v;
}

export function decodeCbor(hex: string): Cbor {
  if (!/^(?:[0-9a-fA-F]{2})*$/.test(hex)) fail('not hex');
  const reader = new Reader(Buffer.from(hex, 'hex'));
  const value = reader.read();
  if (!reader.done()) fail('trailing bytes');
  return value;
}

/* ------------------------------------------------------------------------- */
/* Transaction views                                                          */
/* ------------------------------------------------------------------------- */

export interface CborAnchor {
  url: string;
  dataHash: string;
}

/** Tag 258 marks a set; its content is the array. */
const list = (v: Cbor | undefined): Cbor[] => {
  const inner = v instanceof CborTag && Number(v.tag) === 258 ? v.value : v;
  if (inner === undefined) return [];
  return Array.isArray(inner) ? inner : fail('expected a list');
};

const bytesHex = (v: Cbor | undefined, len: number, what: string): string =>
  Buffer.isBuffer(v) && v.length === len ? v.toString('hex') : fail(`${what} is not ${len} bytes`);

const int = (v: Cbor | undefined, what: string): number =>
  typeof v === 'number' && Number.isInteger(v) ? v : fail(`${what} is not an integer`);

function anchorOf(v: Cbor | undefined): CborAnchor | null {
  if (v === null || v === undefined) return null;
  if (!Array.isArray(v) || typeof v[0] !== 'string') return fail('anchor is not [url, hash]');
  return { url: v[0], dataHash: bytesHex(v[1], 32, 'anchor hash') };
}

function body(tx: Cbor): CborMap {
  const b = Array.isArray(tx) ? tx[0] : undefined;
  return b instanceof CborMap ? b : fail('transaction has no body');
}

/** Voter kinds of the Conway CDDL: 0/1 committee hot key/script, 2/3 DRep key/script, 4 pool. */
export type CborVoterKind = 'ccHot' | 'drep' | 'spo';

export interface CborVote {
  voter: { kind: CborVoterKind; hash: string; isScript: boolean };
  action: { txHash: string; index: number };
  /** 0 No, 1 Yes, 2 Abstain in the CDDL. */
  vote: 'no' | 'yes' | 'abstain';
  anchor: CborAnchor | null;
}

const VOTES = ['no', 'yes', 'abstain'] as const;

/** Every voting procedure in the body (key 19), in wire order. */
export function votesOf(tx: Cbor): CborVote[] {
  const procedures = body(tx).get(19);
  if (procedures === undefined) return [];
  if (!(procedures instanceof CborMap)) return fail('voting procedures are not a map');
  const out: CborVote[] = [];
  for (const [voterCbor, actions] of procedures.entries) {
    if (!Array.isArray(voterCbor)) return fail('voter is not [kind, hash]');
    const tag = int(voterCbor[0], 'voter kind');
    const kind: CborVoterKind | undefined = tag <= 1 ? 'ccHot' : tag <= 3 ? 'drep' : tag === 4 ? 'spo' : undefined;
    if (!kind) return fail(`unknown voter kind ${tag}`);
    const voter = { kind, hash: bytesHex(voterCbor[1], 28, 'voter hash'), isScript: tag === 1 || tag === 3 };
    if (!(actions instanceof CborMap)) return fail('votes are not a map');
    for (const [actionCbor, procedure] of actions.entries) {
      if (!Array.isArray(actionCbor) || !Array.isArray(procedure)) return fail('malformed voting procedure');
      const vote = VOTES[int(procedure[0], 'vote')];
      if (!vote) return fail('unknown vote');
      out.push({
        voter,
        action: { txHash: bytesHex(actionCbor[0], 32, 'action tx hash'), index: int(actionCbor[1], 'action index') },
        vote,
        anchor: anchorOf(procedure[1]),
      });
    }
  }
  return out;
}

/** The anchor of each proposal procedure (key 20), by index in the transaction. */
export function proposalAnchorsOf(tx: Cbor): CborAnchor[] {
  return list(body(tx).get(20)).map((p) => {
    if (!Array.isArray(p)) return fail('proposal procedure is not a list');
    return anchorOf(p[3]) ?? fail('proposal procedure has no anchor');
  });
}

/**
 * The anchor a DRep registration (16) or update (18) certificate sets, by the
 * certificate's index in the body (key 4). `null` = registered or updated
 * with no anchor; any other certificate type at that index throws.
 */
export function drepCertAnchorOf(tx: Cbor, certIndex: number): CborAnchor | null {
  const cert = list(body(tx).get(4))[certIndex];
  if (!Array.isArray(cert)) return fail(`no certificate at index ${certIndex}`);
  const type = int(cert[0], 'certificate type');
  if (type === 16) return anchorOf(cert[3]);
  if (type === 18) return anchorOf(cert[2]);
  return fail(`certificate ${certIndex} is type ${type}, not a DRep registration or update`);
}
