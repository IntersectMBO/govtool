import { afterEach, describe, expect, it } from 'vitest';

import {
    adaToLovelace,
    getGovActionDepositAda,
    getLovelaceFromCborBalance,
    getHardForkContent,
    isBlankValue,
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

describe('adaToLovelace', () => {
    it('converts whole and decimal amounts without float rounding', () => {
        expect(adaToLovelace(929)).toBe('929000000');
        expect(adaToLovelace('1.005')).toBe('1005000');
        expect(adaToLovelace('0.000001')).toBe('1');
        expect(adaToLovelace('12,5')).toBe('12500000');
        expect(adaToLovelace('0')).toBe('0');
    });

    it('refuses amounts the ledger cannot carry', () => {
        expect(() => adaToLovelace('-1')).toThrow();
        expect(() => adaToLovelace('1.0000001')).toThrow();
        expect(() => adaToLovelace('abc')).toThrow();
        expect(() => adaToLovelace('')).toThrow();
    });
});

describe('isBlankValue', () => {
    it('counts zero as a value', () => {
        expect(isBlankValue(0)).toBe(false);
        expect(isBlankValue('0')).toBe(false);
    });

    it('counts missing and empty values as blank', () => {
        expect(isBlankValue(undefined)).toBe(true);
        expect(isBlankValue(null)).toBe(true);
        expect(isBlankValue('')).toBe(true);
        expect(isBlankValue('  ')).toBe(true);
    });
});

describe('getHardForkContent', () => {
    it('reads the relation the API returns', () => {
        expect(
            getHardForkContent({
                data: {
                    id: 5,
                    attributes: {
                        previous_ga_hash: null,
                        previous_ga_id: null,
                        major: '11',
                        minor: '0',
                    },
                },
            })
        ).toEqual({
            previous_ga_hash: null,
            previous_ga_id: null,
            major: '11',
            minor: '0',
        });
    });

    it('passes the flat form fields through', () => {
        expect(getHardForkContent({ major: '10', minor: '1' })).toEqual({
            major: '10',
            minor: '1',
        });
    });

    it('answers no fields for an empty relation or none', () => {
        expect(getHardForkContent({ data: null })).toEqual({});
        expect(getHardForkContent(undefined)).toEqual({});
    });
});
