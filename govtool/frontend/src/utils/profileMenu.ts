// The top nav's profile popup can be opened from elsewhere (the wallet popup's
// View Profile, the connection modal's View your profile). The avatar's menu
// listens while it is on screen; a request nothing hears is reported as such,
// so the caller can fall back (phones show no avatar).

type Listener = () => void;

let listener: Listener | null = null;

/** Registers the open handler of the mounted profile menu. */
export const subscribeProfileMenu = (open: Listener) => {
  listener = open;
  return () => {
    if (listener === open) listener = null;
  };
};

/** Opens the profile menu; false when no menu is on screen to open. */
export const requestProfileMenu = () => {
  if (!listener) return false;
  listener();
  return true;
};

const HEX = /^(?:[0-9a-f]{2})+$/i;

/**
 * The lovelace in a CIP-30 getBalance answer: a CBOR value that is either a
 * coin (an unsigned integer) or [coin, multiasset]. Undefined for anything
 * else.
 */
export const lovelaceFromCborValue = (hex?: string) => {
  if (!hex || !HEX.test(hex)) return undefined;
  const bytes = hex.match(/../g)!.map((byte) => parseInt(byte, 16));
  // A two-item array holds the coin first.
  const at = bytes[0] === 0x82 ? 1 : 0;
  const head = bytes[at];
  // Major type 0 (an unsigned integer) is the top three bits being zero.
  if (head === undefined || head >= 0x20) return undefined;
  const info = head;
  if (info < 24) return info;
  const width = { 24: 1, 25: 2, 26: 4, 27: 8 }[info];
  if (!width || bytes.length < at + 1 + width) return undefined;
  return bytes
    .slice(at + 1, at + 1 + width)
    .reduce((value, byte) => value * 256 + byte, 0);
};

type ActionKey = { txHash: string; index: number };

/**
 * How many live actions a voter has not voted on; undefined while the live
 * list is only a first page or the votes are unknown.
 */
export const pendingVoteCount = (
  live: ActionKey[],
  liveTotal: number,
  votes?: { proposal: ActionKey }[],
) => {
  if (!votes || live.length < liveTotal) return undefined;
  const voted = new Set(votes.map(({ proposal }) => `${proposal.txHash}#${proposal.index}`));
  return live.filter(({ txHash, index }) => !voted.has(`${txHash}#${index}`)).length;
};
