import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

vi.mock('axios');
vi.mock('../api', () => ({
    getChallenge: vi.fn(),
    loginUser: vi.fn(),
    getLoggedInUserInfo: vi.fn(),
    getRefreshToken: vi.fn(),
}));

import {
    getChallenge,
    getLoggedInUserInfo,
    getRefreshToken,
    loginUser,
} from '../api';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    loginUserToApp,
} from '../helpers';
import { getDataFromSession, saveDataInSession } from '../utils';

const makeJwt = (claims) => `h.${btoa(JSON.stringify(claims))}.s`;
const storeJwt = (claims) => saveDataInSession('pdfUserJwt', makeJwt(claims));

const baseArgs = () => ({
    setUser: vi.fn(),
    setOpenUsernameModal: vi.fn(),
    clearStates: vi.fn(),
    addErrorAlert: vi.fn(),
    addSuccessAlert: vi.fn(),
    addChangesSavedAlert: vi.fn(),
});

const wallet = (overrides = {}) => ({
    address: 'addr1',
    stakeKey: 'stake1',
    dRepID: 'drep1',
    cip95: { signData: vi.fn().mockResolvedValue({ signature: 'sig' }) },
    ...overrides,
});

beforeEach(() => {
    vi.clearAllMocks();
    sessionStorage.clear();
    getChallenge.mockResolvedValue({ message: 'challenge' });
    loginUser.mockResolvedValue({
        jwt: makeJwt({ stakeKey: 'stake1' }),
        user: { govtool_username: 'alice' },
    });
    getLoggedInUserInfo.mockResolvedValue({ govtool_username: 'alice' });
});

afterEach(() => sessionStorage.clear());

describe('loginUserToApp: sign-in guards', () => {
    it('sends no challenge without a stake key (D2)', async () => {
        const args = baseArgs();
        await loginUserToApp({
            ...args,
            wallet: wallet({ stakeKey: undefined }),
            trigerSignData: true,
        });
        expect(getChallenge).not.toHaveBeenCalled();
        expect(args.clearStates).not.toHaveBeenCalled();
    });

    it('sends no challenge with a null wallet', async () => {
        await loginUserToApp({
            ...baseArgs(),
            wallet: null,
            trigerSignData: true,
        });
        expect(getChallenge).not.toHaveBeenCalled();
    });

    it('signs with the stake key and saves the JWT', async () => {
        const args = baseArgs();
        const w = wallet();
        await loginUserToApp({ ...args, wallet: w, trigerSignData: true });
        expect(getChallenge).toHaveBeenCalledWith({
            query: '?identifier=stake1',
        });
        expect(w.cip95.signData).toHaveBeenCalledWith('stake1', expect.any(String));
        expect(loginUser).toHaveBeenCalledWith(
            expect.objectContaining({ identifier: 'stake1' })
        );
        expect(getDataFromSession('pdfUserJwt')).toBe(
            makeJwt({ stakeKey: 'stake1' })
        );
        expect(args.addSuccessAlert).toHaveBeenCalledWith(
            'Successfully signed data with stake key.'
        );
    });

    it('sends no DRep challenge without a dRepID', async () => {
        await loginUserToApp({
            ...baseArgs(),
            wallet: wallet({ dRepID: '' }),
            isDRep: true,
        });
        expect(getChallenge).not.toHaveBeenCalled();
    });

    it('signs with the dRepID for a DRep sign-in', async () => {
        await loginUserToApp({ ...baseArgs(), wallet: wallet(), isDRep: true });
        expect(getChallenge).toHaveBeenCalledWith({
            query: '?identifier=drep1',
        });
    });
});

describe('loginUserToApp: session restore', () => {
    it('keeps a JWT for the same stake key', async () => {
        storeJwt({ stakeKey: 'stake1' });
        const args = baseArgs();
        await loginUserToApp({ ...args, wallet: wallet(), trigerSignData: false });
        expect(getLoggedInUserInfo).toHaveBeenCalledTimes(1);
        expect(args.setUser).toHaveBeenCalledWith({
            user: { govtool_username: 'alice' },
        });
        expect(args.addSuccessAlert).toHaveBeenCalledWith(
            'Successfully logged in.'
        );
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });

    it('clears a JWT for another stake key', async () => {
        storeJwt({ stakeKey: 'stake1' });
        const args = baseArgs();
        await loginUserToApp({
            ...args,
            wallet: wallet({ stakeKey: 'stake2' }),
            trigerSignData: false,
        });
        expect(getLoggedInUserInfo).not.toHaveBeenCalled();
        expect(args.clearStates).toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('keeps a DRep JWT for the same dRepID', async () => {
        storeJwt({ stakeKey: 'stake1', dRepID: 'drep1' });
        await loginUserToApp({
            ...baseArgs(),
            wallet: wallet(),
            trigerSignData: false,
        });
        expect(getLoggedInUserInfo).toHaveBeenCalledTimes(1);
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });

    it('clears a DRep JWT for another dRepID', async () => {
        storeJwt({ stakeKey: 'stake1', dRepID: 'drep1' });
        await loginUserToApp({
            ...baseArgs(),
            wallet: wallet({ dRepID: 'drep2' }),
            trigerSignData: false,
        });
        expect(getLoggedInUserInfo).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('keeps the JWT when there is no wallet yet (D6)', async () => {
        storeJwt({ stakeKey: 'stake1', dRepID: 'drep1' });
        const args = baseArgs();
        await loginUserToApp({ ...args, wallet: null, trigerSignData: false });
        await loginUserToApp({ ...args, wallet: undefined });
        expect(args.clearStates).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });

    it('refreshes once when users/me fails, then restores', async () => {
        storeJwt({ stakeKey: 'stake1' });
        getLoggedInUserInfo
            .mockResolvedValueOnce(undefined)
            .mockResolvedValueOnce({ govtool_username: 'alice' });
        getRefreshToken.mockResolvedValue({
            jwt: makeJwt({ stakeKey: 'stake1', exp: 2 }),
        });
        const args = baseArgs();
        await loginUserToApp({ ...args, wallet: wallet(), trigerSignData: false });
        expect(getRefreshToken).toHaveBeenCalledTimes(1);
        expect(args.setUser).toHaveBeenCalledWith({
            user: { govtool_username: 'alice' },
        });
        expect(getDataFromSession('pdfUserJwt')).toBe(
            makeJwt({ stakeKey: 'stake1', exp: 2 })
        );
    });

    it('signs out cleanly when users/me and the refresh both fail', async () => {
        storeJwt({ stakeKey: 'stake1' });
        getLoggedInUserInfo.mockResolvedValue(undefined);
        getRefreshToken.mockRejectedValue(new Error('401'));
        const args = baseArgs();
        await loginUserToApp({ ...args, wallet: wallet(), trigerSignData: false });
        expect(args.setUser).not.toHaveBeenCalled();
        expect(args.setOpenUsernameModal).not.toHaveBeenCalled();
        expect(args.clearStates).toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('drops a refreshed JWT that belongs to another stake key', async () => {
        storeJwt({ stakeKey: 'stake1' });
        getLoggedInUserInfo.mockResolvedValue(undefined);
        getRefreshToken.mockResolvedValue({
            jwt: makeJwt({ stakeKey: 'stake2' }),
        });
        const args = baseArgs();
        await loginUserToApp({ ...args, wallet: wallet(), trigerSignData: false });
        expect(getLoggedInUserInfo).toHaveBeenCalledTimes(1);
        expect(args.setUser).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('does nothing after the await once the run is stale', async () => {
        storeJwt({ stakeKey: 'stake1' });
        let stale = false;
        getLoggedInUserInfo.mockImplementation(async () => {
            stale = true;
            return undefined;
        });
        const args = baseArgs();
        await loginUserToApp({
            ...args,
            wallet: wallet(),
            trigerSignData: false,
            isStale: () => stale,
        });
        expect(getRefreshToken).not.toHaveBeenCalled();
        expect(args.setUser).not.toHaveBeenCalled();
        expect(args.clearStates).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });

    it('does not sign when a JWT is present', async () => {
        storeJwt({ stakeKey: 'stake1' });
        await loginUserToApp({ ...baseArgs(), wallet: wallet() });
        expect(getChallenge).not.toHaveBeenCalled();
    });
});

describe('checkIfDrepIsSignedIn / checkShowValidation', () => {
    const user = { user: { govtool_username: 'alice' } };
    const dRepWallet = (voter) => wallet({ voter });

    it.each([
        [{ isRegisteredAsDRep: true }, false, true],
        [{ isRegisteredAsSoleVoter: true }, false, true],
        [{ isRegisteredAsDRep: true }, true, false],
        [{}, false, false],
        [undefined, false, false],
    ])('voter %o, JWT dRepID %s -> %s', (voter, jwtHasDRep, expected) => {
        storeJwt(
            jwtHasDRep
                ? { stakeKey: 'stake1', dRepID: 'drep1' }
                : { stakeKey: 'stake1' }
        );
        expect(checkIfDrepIsSignedIn(dRepWallet(voter))).toBe(expected);
        expect(checkShowValidation(true, dRepWallet(voter), user)).toBe(
            expected
        );
    });

    it('shows validation without a wallet, user or username', () => {
        expect(checkShowValidation(false, null, user)).toBe(true);
        expect(checkShowValidation(false, wallet(), null)).toBe(true);
        expect(checkShowValidation(false, wallet(), { user: {} })).toBe(true);
        expect(checkShowValidation(false, wallet(), user)).toBe(false);
    });
});
