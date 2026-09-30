import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';
import { act, fireEvent, render, screen } from '@testing-library/react';

vi.mock('axios');
vi.mock('../../lib/api', () => ({
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
} from '../../lib/api';
import { AppContextProvider, useAppContext } from '../context';
import { checkIfDrepIsSignedIn } from '../../lib/helpers';
import { getDataFromSession, saveDataInSession } from '../../lib/utils';
import UserValidation from '../../components/UserValidation/UserValidation';

const makeJwt = (claims) => `h.${btoa(JSON.stringify(claims))}.s`;
const storeJwt = (claims) => saveDataInSession('pdfUserJwt', makeJwt(claims));
const inMinutes = (m) => Math.floor(Date.now() / 1000) + m * 60;

const makeWallet = (overrides = {}) => ({
    address: 'addr1',
    isEnabled: true,
    stakeKey: 'stake1',
    dRepID: 'drep1',
    cip95: { signData: vi.fn().mockResolvedValue({ signature: 'sig' }) },
    ...overrides,
});

const hostProps = (overrides = {}) => ({
    walletStatus: 'ready',
    walletAPI: makeWallet(),
    addSuccessAlert: vi.fn(),
    addErrorAlert: vi.fn(),
    addWarningAlert: vi.fn(),
    addChangesSavedAlert: vi.fn(),
    setUsername: vi.fn(),
    ...overrides,
});

let seen;
const Probe = () => {
    const ctx = useAppContext();
    seen = ctx;
    return (
        <div>
            <span data-testid='probe-user'>
                {ctx.user ? ctx.user.user?.govtool_username ?? 'anon' : 'none'}
            </span>
            <button data-testid='probe-clear' onClick={ctx.clearStates} />
        </div>
    );
};

const renderProvider = (props, children = <Probe />) => {
    const utils = render(
        <AppContextProvider govtoolProps={props}>{children}</AppContextProvider>
    );
    return {
        ...utils,
        rerenderWith: (next, nextChildren = children) =>
            utils.rerender(
                <AppContextProvider govtoolProps={next}>
                    {nextChildren}
                </AppContextProvider>
            ),
    };
};

// Lets pending promises from effects settle.
const flush = () => act(async () => {});

beforeEach(() => {
    vi.clearAllMocks();
    sessionStorage.clear();
    seen = undefined;
    getLoggedInUserInfo.mockResolvedValue({ govtool_username: 'alice' });
    getChallenge.mockResolvedValue({ message: 'challenge' });
    loginUser.mockResolvedValue({
        jwt: makeJwt({ stakeKey: 'stake1', exp: inMinutes(60) }),
        user: { govtool_username: 'alice' },
    });
});

afterEach(() => {
    vi.useRealTimers();
    sessionStorage.clear();
});

describe('live host values', () => {
    it('sees a late voter in the same commit (D9)', () => {
        const props = hostProps();
        const { rerenderWith } = renderProvider(props);
        expect(seen.walletAPI.voter).toBeUndefined();

        const voter = { isRegisteredAsDRep: true, votingPower: 5 };
        rerenderWith({
            ...props,
            walletAPI: { ...props.walletAPI, voter },
        });
        expect(seen.walletAPI.voter).toBe(voter);
    });

    it('sees new epochParams with no lag', () => {
        const props = hostProps({ epochParams: { gov_action_deposit: 1 } });
        const { rerenderWith } = renderProvider(props);
        expect(seen.epochParams).toEqual({ gov_action_deposit: 1 });
        rerenderWith({ ...props, epochParams: { gov_action_deposit: 2 } });
        expect(seen.epochParams).toEqual({ gov_action_deposit: 2 });
    });

    it('has the alert functions on the first render', () => {
        const props = hostProps();
        renderProvider(props);
        expect(seen.addSuccessAlert).toBe(props.addSuccessAlert);
        expect(seen.addErrorAlert).toBe(props.addErrorAlert);
        expect(seen.addWarningAlert).toBe(props.addWarningAlert);
        expect(seen.addChangesSavedAlert).toBe(props.addChangesSavedAlert);
    });

    it('hides the wallet until it is ready', () => {
        renderProvider(hostProps({ walletStatus: 'connecting' }));
        expect(seen.walletAPI).toBeNull();
        expect(seen.walletStatus).toBe('connecting');
    });

    it('never infers disconnected when the host omits walletStatus', () => {
        storeJwt({ stakeKey: 'stake1' });
        renderProvider(
            hostProps({
                walletStatus: undefined,
                walletAPI: makeWallet({ stakeKey: undefined }),
            })
        );
        expect(seen.walletStatus).toBe('connecting');
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });
});

describe('session sync', () => {
    it('keeps the JWT while connecting and clears it when disconnected (D6)', async () => {
        storeJwt({ stakeKey: 'stake1', dRepID: 'drep1' });
        const props = hostProps({ walletStatus: 'connecting' });
        const { rerenderWith } = renderProvider(props);

        // R3/R4: address set, stake key and dRepID not yet.
        for (const walletAPI of [
            makeWallet({ stakeKey: undefined, dRepID: '' }),
            makeWallet({ dRepID: '' }),
            makeWallet({ address: undefined, isEnabled: false }),
        ]) {
            rerenderWith({ ...props, walletAPI });
            await flush();
        }
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
        expect(getLoggedInUserInfo).not.toHaveBeenCalled();

        rerenderWith({ ...props, walletStatus: 'disconnected' });
        await flush();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('restores the JWT once when the wallet becomes ready', async () => {
        storeJwt({ stakeKey: 'stake1' });
        const props = hostProps({ walletStatus: 'connecting' });
        const { rerenderWith } = renderProvider(props);
        await flush();

        for (let i = 0; i < 6; i += 1) {
            rerenderWith({
                ...props,
                walletStatus: 'ready',
                walletAPI: makeWallet(),
            });
            await flush();
        }
        expect(getLoggedInUserInfo).toHaveBeenCalledTimes(1);
        expect(props.addSuccessAlert).toHaveBeenCalledTimes(1);
        expect(props.addSuccessAlert).toHaveBeenCalledWith(
            'Successfully logged in.'
        );
        expect(screen.getByTestId('probe-user')).toHaveTextContent('alice');
    });

    it('clears a JWT that belongs to another stake key', async () => {
        storeJwt({ stakeKey: 'stake1' });
        renderProvider(
            hostProps({ walletAPI: makeWallet({ stakeKey: 'stake2' }) })
        );
        await flush();
        expect(getLoggedInUserInfo).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('re-checks the session when the stake key switches', async () => {
        storeJwt({ stakeKey: 'stake1' });
        const props = hostProps();
        const { rerenderWith } = renderProvider(props);
        await flush();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('alice');

        rerenderWith({
            ...props,
            walletAPI: makeWallet({ stakeKey: 'stake2' }),
        });
        await flush();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('none');
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('re-checks a DRep JWT when the dRepID changes after ready', async () => {
        storeJwt({ stakeKey: 'stake1', dRepID: 'drep1' });
        const props = hostProps();
        const { rerenderWith } = renderProvider(props);
        await flush();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('alice');

        rerenderWith({ ...props, walletAPI: makeWallet({ dRepID: 'drep2' }) });
        await flush();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('none');
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('ignores a restore that finishes after a disconnect', async () => {
        storeJwt({ stakeKey: 'stake1' });
        let resolveMe;
        getLoggedInUserInfo.mockImplementation(
            () => new Promise((r) => (resolveMe = r))
        );
        const props = hostProps();
        const { rerenderWith } = renderProvider(props);
        await flush();
        rerenderWith({ ...props, walletStatus: 'disconnected' });
        await flush();
        await act(async () => resolveMe({ govtool_username: 'alice' }));
        expect(screen.getByTestId('probe-user')).toHaveTextContent('none');
        expect(props.addSuccessAlert).not.toHaveBeenCalled();
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('does nothing when ready without a JWT', async () => {
        renderProvider(hostProps());
        await flush();
        expect(getLoggedInUserInfo).not.toHaveBeenCalled();
        expect(getChallenge).not.toHaveBeenCalled();
    });
});

describe('refresh loop', () => {
    const signedIn = async (claims, extraChildren = null) => {
        storeJwt(claims);
        const props = hostProps();
        const utils = renderProvider(
            props,
            <>
                <Probe />
                {extraChildren}
            </>
        );
        await flush();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('alice');
        return { props, ...utils };
    };

    it('refreshes once per tick with several UserValidation mounted', async () => {
        vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] });
        getRefreshToken.mockResolvedValue({
            jwt: makeJwt({ stakeKey: 'stake1', exp: inMinutes(60) }),
        });
        await signedIn(
            { stakeKey: 'stake1', exp: inMinutes(4) },
            <>
                <UserValidation />
                <UserValidation type='comment' />
                <UserValidation type='proposal' />
            </>
        );

        await act(async () => {
            await vi.advanceTimersByTimeAsync(60 * 1000);
        });
        expect(getRefreshToken).toHaveBeenCalledTimes(1);
        expect(screen.getByTestId('probe-user')).toHaveTextContent('alice');
    });

    it('logs out when the refreshed JWT is for another stake key', async () => {
        vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] });
        getRefreshToken.mockResolvedValue({
            jwt: makeJwt({ stakeKey: 'stake2', exp: inMinutes(60) }),
        });
        await signedIn({ stakeKey: 'stake1', exp: inMinutes(4) });

        await act(async () => {
            await vi.advanceTimersByTimeAsync(60 * 1000);
        });
        expect(screen.getByTestId('probe-user')).toHaveTextContent('none');
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });

    it('logs out when the JWT has expired', async () => {
        vi.useFakeTimers({ toFake: ['setInterval', 'clearInterval'] });
        await signedIn({ stakeKey: 'stake1', exp: inMinutes(-1) });

        await act(async () => {
            await vi.advanceTimersByTimeAsync(60 * 1000);
        });
        expect(getRefreshToken).not.toHaveBeenCalled();
        expect(screen.getByTestId('probe-user')).toHaveTextContent('none');
        expect(getDataFromSession('pdfUserJwt')).toBeNull();
    });
});

describe('UserValidation', () => {
    it('offers to connect while the wallet is connecting', () => {
        renderProvider(
            hostProps({
                walletStatus: 'connecting',
                walletAPI: makeWallet({ stakeKey: undefined }),
            }),
            <UserValidation />
        );
        expect(screen.getByTestId('connect-wallet-link')).toHaveTextContent(
            'connect a Cardano wallet'
        );
        expect(screen.queryByTestId('verify-user-link')).toBeNull();
    });

    it('signs in with the stake key once ready', async () => {
        const props = hostProps();
        renderProvider(props, <UserValidation />);
        const link = screen.getByTestId('verify-user-link');
        expect(link).toHaveTextContent(
            'verify yourself by signing a transaction'
        );

        fireEvent.click(link);
        await flush();
        expect(getChallenge).toHaveBeenCalledWith({
            query: '?identifier=stake1',
        });
        expect(props.walletAPI.cip95.signData).toHaveBeenCalledWith(
            'stake1',
            expect.any(String)
        );
        expect(getDataFromSession('pdfUserJwt')).not.toBeNull();
    });

    it('shows verify-drep-link when the voter arrives late (D9)', async () => {
        storeJwt({ stakeKey: 'stake1', exp: inMinutes(60) });
        const Poll = () => {
            const { walletAPI } = useAppContext();
            return (
                <UserValidation
                    type='drep-poll'
                    drepCheck={checkIfDrepIsSignedIn(walletAPI)}
                    drepRequired
                />
            );
        };
        const props = hostProps();
        const { rerenderWith } = renderProvider(props, <Poll />);
        await flush();
        expect(screen.queryByTestId('verify-drep-link')).toBeNull();

        rerenderWith({
            ...props,
            walletAPI: {
                ...props.walletAPI,
                voter: { isRegisteredAsDRep: true },
            },
        });
        expect(screen.getByTestId('verify-drep-link')).toHaveTextContent(
            'verify your status as a DRep.'
        );

        fireEvent.click(screen.getByTestId('verify-drep-link'));
        await flush();
        expect(getChallenge).toHaveBeenCalledWith({
            query: '?identifier=drep1',
        });
    });
});

describe('username mirror', () => {
    it('mirrors the username to the host and clears it on logout', async () => {
        storeJwt({ stakeKey: 'stake1' });
        const props = hostProps();
        renderProvider(props);
        await flush();
        expect(props.setUsername).toHaveBeenLastCalledWith('alice');

        fireEvent.click(screen.getByTestId('probe-clear'));
        await flush();
        expect(props.setUsername).toHaveBeenLastCalledWith('');
    });

    it('mirrors a username set after a fresh sign-in', async () => {
        const props = hostProps();
        renderProvider(
            props,
            <>
                <Probe />
                <UserValidation />
            </>
        );
        fireEvent.click(screen.getByTestId('verify-user-link'));
        await flush();
        expect(props.setUsername).toHaveBeenLastCalledWith('alice');
    });
});
