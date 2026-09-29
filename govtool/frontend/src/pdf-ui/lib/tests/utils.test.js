import { afterEach, describe, expect, it } from 'vitest';

import {
    getGovActionDepositAda,
    getLovelaceFromCborBalance,
    isValidURLFormat,
    setUrlPortsAllowed,
} from '../utils';

describe('getLovelaceFromCborBalance', () => {
    it('reads a coin encoded in one byte', () => {
        expect(getLovelaceFromCborBalance('05')).toBe(5);
    });

    it('reads a coin encoded as a 4-byte uint', () => {
        expect(getLovelaceFromCborBalance('1a3cea7b80')).toBe(1022000000);
    });

    it('reads a coin encoded as an 8-byte uint', () => {
        expect(getLovelaceFromCborBalance('1b000000174879a720')).toBe(
            100000180000
        );
    });

    it('reads the coin of a [coin, multiasset] value', () => {
        expect(
            getLovelaceFromCborBalance(
                '821a3cea7b80a1581c00000000000000000000000000000000000000000000000000000000a143746f6b07'
            )
        ).toBe(1022000000);
    });

    it('returns 0 for invalid input', () => {
        expect(getLovelaceFromCborBalance('zz')).toBe(0);
        expect(getLovelaceFromCborBalance(undefined)).toBe(0);
    });
});

describe('getGovActionDepositAda', () => {
    it('converts gov_action_deposit from lovelace to ADA', () => {
        expect(getGovActionDepositAda({ gov_action_deposit: 1000000000 })).toBe(
            1000
        );
    });

    it('returns null when the deposit is unknown', () => {
        expect(getGovActionDepositAda(undefined)).toBeNull();
        expect(getGovActionDepositAda({ gov_action_deposit: null })).toBeNull();
    });
});

describe('isValidURLFormat', () => {
    afterEach(() => setUrlPortsAllowed(false));

    it('rejects a port by default', () => {
        expect(isValidURLFormat('http://127.0.0.1:3001/doc.json')).toBe(false);
        expect(isValidURLFormat('https://example.com/doc.json')).toBe(true);
    });

    it('accepts a port from 1 to 65535 when ports are allowed', () => {
        setUrlPortsAllowed(true);
        expect(isValidURLFormat('http://127.0.0.1:3001/doc.json')).toBe(true);
        expect(isValidURLFormat('https://example.com:65535')).toBe(true);
        expect(isValidURLFormat('https://example.com:65536')).toBe(false);
        expect(isValidURLFormat('https://example.com:0/doc')).toBe(false);
        expect(isValidURLFormat('ipfs://abc')).toBe(true);
    });
});
