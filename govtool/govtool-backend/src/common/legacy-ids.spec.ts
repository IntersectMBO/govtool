import { BadRequestException } from '@nestjs/common';
import { bech32 } from 'bech32';
import { ChainDataError } from '@govtool/data-providers/chain-data';

import {
  decodeCip129DRepId,
  drepIdToCip105,
  drepIdToHex,
  firstFound,
  legacyDRepCandidates,
  legacyGovActionId,
  legacyStakeAddress,
  tryLegacyDRepCandidates,
  tryLegacyGovActionId,
} from './legacy-ids';

/*
 * Vectors from preview db-sync (`drep_hash.raw`/`view`, `stake_address.hash_raw`
 * /`view`), with the CIP-129 ids cross-checked against the db-sync provider's
 * own encoder, an independent implementation.
 */
const KEY_HASH = 'f0ed00410031f3288d7889aa896cfdad79a7441885d3bae8982ac151';
const KEY_CIP105 = 'drep17rksqsgqx8ej3rtc3x4gjm8a44u6w3qcshfm46yc9tq4zeffuzj';
const KEY_CIP129 = 'drep1ytcw6qzpqqclx2yd0zy64ztvlkkhnf6yrzza8whgnq4vz5gh89626';

const SCRIPT_HASH = '186e32faa80a26810392fda6d559c7ed4721a65ce1c9d4ef3e1c87b4';
/** db-sync's `view` for this script DRep, wrongly with the key prefix. */
const SCRIPT_DBSYNC_VIEW =
  'drep1rphr974gpgngzqujlknd2kw8a4rjrfjuu8yafme7rjrmg443jff';
const SCRIPT_CIP129 =
  'drep1yvvxuvh64q9zdqgrjt76d42eclk5wgdxtnsun4808cwg0dquhj65s';

const TX = 'd'.repeat(64);
const TX_ACTION_ID =
  'gov_action1mhwamhwamhwamhwamhwamhwamhwamhwamhwamhwamhwamhwamhwsqc8rypj';

const STAKE_HEX = 'e09ae5bf7dedec9a938612d7b90922068f8f2a96c5c4c59399cd73d044';
const STAKE_BECH32 =
  'stake_test1uzdwt0maahkf4yuxzttmjzfzq68c725kchzvtyuee4eaq3qscx27n';
const SCRIPT_STAKE_HEX =
  'f0997864499a640ec62caf2eacf6e0549d0b638c1e80c1528d219dd683';
const SCRIPT_STAKE_BECH32 =
  'stake_test17zvhsezfnfjqa33v4uh2eahq2jwskcuvr6qvz55dyxwadqcltz7ff';

function expect400(action: () => unknown): void {
  try {
    action();
  } catch (error) {
    expect(error).toBeInstanceOf(BadRequestException);
    expect((error as BadRequestException).getResponse()).toMatchObject({
      errorType: 'ValidationError',
    });
    return;
  }
  throw new Error('expected a 400');
}

describe('DRep ids', () => {
  it('passes a CIP-129 id through', () => {
    expect(legacyDRepCandidates(KEY_CIP129)).toEqual([KEY_CIP129]);
    expect(legacyDRepCandidates(SCRIPT_CIP129)).toEqual([SCRIPT_CIP129]);
  });

  it('canonicalises an all-uppercase CIP-129 id', () => {
    expect(legacyDRepCandidates(KEY_CIP129.toUpperCase())).toEqual([
      KEY_CIP129,
    ]);
  });

  it('decodes CIP-105 rather than string-matching the shared drep1 prefix', () => {
    expect(legacyDRepCandidates(KEY_CIP105)).toEqual([KEY_CIP129]);
    const scriptCip105 = drepIdToCip105(SCRIPT_CIP129);
    expect(scriptCip105.startsWith('drep_script1')).toBe(true);
    expect(legacyDRepCandidates(scriptCip105)).toEqual([SCRIPT_CIP129]);
  });

  it('turns a bare hash into the key id, then the script id', () => {
    expect(legacyDRepCandidates(KEY_HASH)).toEqual([
      KEY_CIP129,
      encodeScriptOf(KEY_HASH),
    ]);
    expect(legacyDRepCandidates(SCRIPT_HASH.toUpperCase())[1]).toBe(
      SCRIPT_CIP129,
    );
  });

  it('reads db-sync’s key-prefixed script view as the key credential it encodes', () => {
    // CIP-105 `drep1…` means a key hash; the prefix is the only type signal,
    // so this resolves to the key id and the lookup is simply NOT_FOUND.
    expect(legacyDRepCandidates(SCRIPT_DBSYNC_VIEW)).toEqual([
      legacyDRepCandidates(SCRIPT_HASH)[0],
    ]);
  });

  it.each([
    '',
    'garbage',
    'a'.repeat(55),
    'a'.repeat(58),
    'zz'.repeat(28),
    // A CIP-129 id with a broken checksum.
    KEY_CIP129.slice(0, -1) + 'x',
    // A pool id: right length, wrong entity.
    'pool1pu5jlj4q9w9jlxeu370a3c9myx47md5j5m2str0naunn2q3lkdy',
    `${'a'.repeat(56)}\n`,
    'drep1' + 'q'.repeat(2000),
  ])('rejects %p with a 400', (input) => {
    expect(tryLegacyDRepCandidates(input)).toBeUndefined();
    expect400(() => legacyDRepCandidates(input));
  });

  it('rejects a 29-byte drep1 id whose header is not a DRep header', () => {
    // Same hash, committee-hot key header (0x02): decodes, wrong entity.
    const bad = encodeWithHeader(0x02, KEY_HASH);
    expect(decodeCip129DRepId(bad)).toBeUndefined();
    expect400(() => legacyDRepCandidates(bad));
  });

  it('renders the legacy output forms from a CIP-129 id', () => {
    expect(drepIdToHex(KEY_CIP129)).toBe(KEY_HASH);
    expect(drepIdToCip105(KEY_CIP129)).toBe(KEY_CIP105);
    expect(drepIdToHex(SCRIPT_CIP129)).toBe(SCRIPT_HASH);
    // The correction the frontend applies to db-sync's view, done here.
    expect(drepIdToCip105(SCRIPT_CIP129)).toBe(
      reprefix(SCRIPT_DBSYNC_VIEW, 'drep_script'),
    );
  });

  it('refuses to render a provider id that is not CIP-129', () => {
    expect(() => drepIdToHex(KEY_CIP105)).toThrow('non-CIP-129');
  });
});

describe('governance action ids', () => {
  it('encodes txHash#index as CIP-129', () => {
    expect(legacyGovActionId(`${TX}#0`)).toEqual({
      txHash: TX,
      index: 0,
      id: TX_ACTION_ID,
    });
    expect(legacyGovActionId(`${TX.toUpperCase()}#0`).id).toBe(TX_ACTION_ID);
  });

  it('passes a CIP-129 id through, with its parts', () => {
    expect(legacyGovActionId(TX_ACTION_ID)).toEqual({
      txHash: TX,
      index: 0,
      id: TX_ACTION_ID,
    });
  });

  it('round-trips the top of the one-byte index', () => {
    const { id } = legacyGovActionId(`${TX}#255`);
    expect(legacyGovActionId(id)).toMatchObject({ txHash: TX, index: 255 });
  });

  it.each([
    `${TX}#256`,
    `${TX}#-1`,
    `${TX}#1.5`,
    `${TX}#`,
    `${TX}`,
    `${'d'.repeat(63)}#0`,
    `${TX}#0#1`,
    `${TX}#0 `,
    'gov_action1abc',
    KEY_CIP129,
  ])('rejects %p with a 400', (input) => {
    expect(tryLegacyGovActionId(input)).toBeUndefined();
    expect400(() => legacyGovActionId(input));
  });
});

describe('stake addresses', () => {
  it('converts the 58-hex reward address the frontend sends', () => {
    expect(legacyStakeAddress(STAKE_HEX)).toBe(STAKE_BECH32);
    expect(legacyStakeAddress(SCRIPT_STAKE_HEX)).toBe(SCRIPT_STAKE_BECH32);
  });

  it('uses the mainnet prefix for network id 1', () => {
    expect(legacyStakeAddress(`e1${'a'.repeat(56)}`)).toBe(
      'stake1ux42424242424242424242424242424242424242424242ser95fn',
    );
  });

  it('reads a bare 56-hex stake key hash as a key reward address on the served network', () => {
    const keyHash = STAKE_HEX.slice(2);
    expect(legacyStakeAddress(keyHash, 'preview')).toBe(STAKE_BECH32);
    expect(legacyStakeAddress(keyHash.toUpperCase(), 'preprod')).toBe(
      STAKE_BECH32,
    );
    expect(legacyStakeAddress('a'.repeat(56), 'mainnet')).toBe(
      'stake1ux42424242424242424242424242424242424242424242ser95fn',
    );
  });

  it('ignores the served network for forms that carry their own', () => {
    expect(legacyStakeAddress(STAKE_HEX, 'mainnet')).toBe(STAKE_BECH32);
    expect(legacyStakeAddress(STAKE_BECH32, 'mainnet')).toBe(STAKE_BECH32);
  });

  it.each(['a'.repeat(55), 'g'.repeat(56), `${'a'.repeat(56)} `, ''])(
    'still rejects %p with a network known',
    (input) => {
      expect400(() => legacyStakeAddress(input, 'mainnet'));
    },
  );

  it('passes bech32 through, canonicalised', () => {
    expect(legacyStakeAddress(STAKE_BECH32)).toBe(STAKE_BECH32);
    expect(legacyStakeAddress(STAKE_BECH32.toUpperCase())).toBe(STAKE_BECH32);
  });

  it.each([
    // A bare credential hash with no served network to read it on.
    'a'.repeat(56),
    // A base-address header (0x00), not a reward address.
    `00${'a'.repeat(56)}`,
    // A reward header with network id 2.
    `e2${'a'.repeat(56)}`,
    // A mainnet header under the testnet prefix.
    reprefix(
      'stake1ux42424242424242424242424242424242424242424242ser95fn',
      'stake_test',
    ),
    KEY_CIP129,
    'stake_test1abc',
    '',
  ])('rejects %p with a 400', (input) => {
    expect400(() => legacyStakeAddress(input));
  });
});

describe('firstFound', () => {
  it('moves to the next candidate only on NOT_FOUND', async () => {
    const asked: string[] = [];
    await expect(
      firstFound(['a', 'b'], (id) => {
        asked.push(id);
        return id === 'a'
          ? Promise.reject(new ChainDataError('NOT_FOUND', id))
          : Promise.resolve(id);
      }),
    ).resolves.toBe('b');
    expect(asked).toEqual(['a', 'b']);
  });

  it('stops at any other failure', async () => {
    const read = jest.fn((id: string) =>
      Promise.reject(new ChainDataError('PROVIDER_UNAVAILABLE', id)),
    );
    await expect(firstFound(['a', 'b'], read)).rejects.toMatchObject({
      code: 'PROVIDER_UNAVAILABLE',
    });
    expect(read).toHaveBeenCalledTimes(1);
  });

  it('rethrows the last NOT_FOUND when nothing matches', async () => {
    await expect(
      firstFound(['a', 'b'], (id) =>
        Promise.reject(new ChainDataError('NOT_FOUND', id)),
      ),
    ).rejects.toMatchObject({ code: 'NOT_FOUND', message: 'b' });
  });
});

/* -- helpers, built on the bech32 library directly rather than the module -- */

function reprefix(id: string, prefix: string): string {
  return bech32.encode(prefix, bech32.decode(id, 1023).words, 1023);
}

function encodeWithHeader(header: number, hash: string): string {
  const bytes = Buffer.concat([
    Buffer.from([header]),
    Buffer.from(hash, 'hex'),
  ]);
  return bech32.encode('drep', bech32.toWords(bytes), 1023);
}

function encodeScriptOf(hash: string): string {
  return encodeWithHeader(0x23, hash);
}
