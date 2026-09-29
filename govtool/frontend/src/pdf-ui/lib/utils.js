import { format } from 'date-fns';
import {
    Address,
    RewardAddress,
    Value,
} from '@emurgo/cardano-serialization-lib-asmjs';

export const URL_REGEX =
    /^(https?:\/\/)(www\.)?(?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,})(?:\/[^\s]*)?$|^(ipfs:\/\/(?:[a-zA-Z0-9]+(?:\/[a-zA-Z0-9._-]+)*))$/;

// Same as URL_REGEX, but http(s) URLs may carry an optional :port.
export const URL_WITH_PORT_REGEX =
    /^(https?:\/\/)(www\.)?(?:\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}|(?:[a-zA-Z0-9-]+\.)+[a-zA-Z]{2,})(?::(?:6553[0-5]|655[0-2]\d|65[0-4]\d{2}|6[0-4]\d{3}|[1-5]\d{4}|[1-9]\d{0,3}))?(?:\/[^\s]*)?$|^(ipfs:\/\/(?:[a-zA-Z0-9]+(?:\/[a-zA-Z0-9._-]+)*))$/;

// Set by GlobalWrapper from the allowUrlPorts prop (GovTool test mode).
let urlPortsAllowed = false;

export const setUrlPortsAllowed = (allowed) => {
    urlPortsAllowed = !!allowed;
};

export function isValidHashFormat(str) {
    return 'Not Implemented';
    // const isValidHash = (hash) => !!bech32.decode(hash)?.words?.length
}
export const formatIsoDate = (isoDate) => {
    if (!isoDate) return '';

    return format(new Date(isoDate), 'd MMMM yyyy');
};

export const formatIsoTime = (isoDate) => {
    if (!isoDate) return '';

    return format(new Date(isoDate), 'hh:mm aa');
};

export const saveDataInSession = (key, value) => {
    const data = { value, timestamp: new Date().getTime() };
    sessionStorage.setItem(key, JSON.stringify(data));
};

export const getDataFromSession = (key) => {
    const data = JSON.parse(sessionStorage.getItem(key));
    if (data) {
        return data.value;
    } else {
        return null;
    }
};

export const clearSession = () => {
    sessionStorage.removeItem('pdfUserJwt');
};

export const utf8ToHex = (str) => {
    return Array.from(str)
        .map((char) => char.charCodeAt(0).toString(16).padStart(2, '0'))
        .join('');
};

export function isValidURLFormat(str) {
    if (!str.length) return false;
    return (urlPortsAllowed ? URL_WITH_PORT_REGEX : URL_REGEX).test(str);
}

export function isValidURLLength(s) {
    if (s.length > 128) {
        return 'Url must be less than 128 bytes';
    }

    const encoder = new TextEncoder();
    const byteLength = encoder.encode(s).length;

    return byteLength <= 128 ? true : 'Url must be less than 128 bytes';
}

export const openInNewTab = (url) => {
    if (url.startsWith('ipfs://')) {
        url = url.replace('ipfs://', 'https://ipfs.io/ipfs/');
    } else if (!url.startsWith('http://') && !url.startsWith('https://')) {
        url = 'https://' + url;
    }
    const newWindow = window.open(url, '_blank', 'noopener,noreferrer');
    if (newWindow) newWindow.opener = null;
};

export async function isRewardAddress(address) {
    try {
        const stake = RewardAddress.from_address(Address.from_bech32(address));
        return stake ? true : 'It must be reward address in bech32 format';
    } catch (e) {
        return 'It must be reward address in bech32 format';
    }
}

/**
 * Validates a string value as a number.
 *
 * @param value - The string value to be validated.
 * @returns Either an error message or `true` if the value is a valid number.
 */
export const numberValidation = (value) => {
    const parsedValue = Number(
        value.includes(',') ? value.replace(',', '.') : value
    );

    if (Number.isNaN(parsedValue)) {
        return 'Only number is allowed';
    }

    if (parsedValue < 0) {
        return 'Only positive number is allowed';
    }

    return true;
};

export const containsString = (str) => {
    return /^(?!\s*$).+/.test(str)
        ? true
        : 'Must contain at least one non-whitespace character.';
};

/**
 * Validates a string value max length.
 *
 * @param str - The string value to be validated.
 * @param limit - The max length value of the string to be validated.
 * @returns Either an error message or `true` if the value has a valid length.
 */
export const maxLengthCheck = (str, limit) => {
    if (typeof str !== 'string') {
        return 'Input must be a string.';
    }

    return str?.length < limit ? true : `Max ${limit} characters.`;
};

export const LOVELACE = 1000000;
const DECIMALS = 6;

export const correctAdaFormat = (lovelace) => {
    if (lovelace) {
        return Number.parseFloat((lovelace / LOVELACE).toFixed(DECIMALS));
    }
    return 0;
};

// Fee margin on top of the governance action deposit, in ADA.
export const GA_SUBMISSION_FEE_ADA = 0.18;

/**
 * Returns the network's governance action deposit in ADA, or null when the
 * epoch params (gov_action_deposit, in lovelace) are not available.
 */
export const getGovActionDepositAda = (epochParams) => {
    const deposit = Number(epochParams?.gov_action_deposit);
    if (!Number.isFinite(deposit) || deposit <= 0) return null;
    return deposit / LOVELACE;
};

/**
 * Returns the lovelace amount of a CIP-30 getBalance() CBOR value, whether it
 * is a plain coin or a [coin, multiasset] pair.
 */
export const getLovelaceFromCborBalance = (cborHex) => {
    try {
        return Number(Value.from_hex(cborHex).coin().to_str()) || 0;
    } catch (error) {
        return 0;
    }
};

export function getItemFromLocalStorage(key) {
    const item = window.localStorage.getItem(key);
    return item ? JSON.parse(item) : null;
}

export const formatDateWithOffset = (
    date,
    utcOffsetHrs,
    formatString,
    timeZoneString
) => {
    if (!date) {
        return '';
    }

    const baseTzOffset = utcOffsetHrs * 60;
    const tzOffset = date.getTimezoneOffset();
    const d = new Date(date.valueOf() + (baseTzOffset + tzOffset) * 60 * 1000);
    return `${format(d, formatString)}${timeZoneString ? ' ' + timeZoneString : ''}`;
};

export function decodeJWT() {
    const jwt = getDataFromSession('pdfUserJwt');

    if (!jwt) {
        return null;
    }

    const payload = jwt?.split('.')[1];
    const decoded = atob(payload.replace(/-/g, '+').replace(/_/g, '/'));
    return JSON.parse(decoded);
}

export const correctVoteAdaFormat = (
    adaAmount = undefined,
    locale = undefined
) => {
    if (adaAmount) {
        const adaNumber = +adaAmount;
        return adaNumber?.toLocaleString(locale, {
            maximumFractionDigits: 3,
        });
    }
    return '0';
};
