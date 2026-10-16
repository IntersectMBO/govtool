import { createContext, useContext, useEffect, useRef, useState } from 'react';
import { loginUserToApp } from '../lib/helpers';
import {
    clearSession,
    decodeJWT,
    getDataFromSession,
    saveDataInSession,
} from '../lib/utils';
import { getRefreshToken } from '../lib/api';

const AppContext = createContext();

const noop = () => {};

// An older host that does not pass walletStatus is never treated as
// disconnected, so it can not clear a session by accident.
const resolveWalletStatus = (govtoolProps) =>
    govtoolProps?.walletStatus ??
    (govtoolProps?.walletAPI?.stakeKey ? 'ready' : 'connecting');

export function AppContextProvider({ children, govtoolProps = {} }) {
    const [user, setUser] = useState();
    const [loading, setLoading] = useState(false);
    const [openUsernameModal, setOpenUsernameModal] = useState({
        open: false,
        callBackFn: () => {},
    });
    const [showIdentificationPage, setShowIdentificationPage] = useState(false);
    const [identificationType, setIdentificationType] = useState('wallet');

    // Host-owned values are read from the current props, never copied.
    const walletStatus = resolveWalletStatus(govtoolProps);
    const walletAPI =
        walletStatus === 'ready' ? govtoolProps.walletAPI ?? null : null;
    const epochParams = govtoolProps.epochParams;
    const locale = govtoolProps.locale ?? 'en';
    const validateMetadata = govtoolProps.validateMetadata ?? null;
    const fetchDRepVotingPowerList =
        govtoolProps.fetchDRepVotingPowerList ?? null;
    const getEnactedProposalDetails =
        govtoolProps.getEnactedProposalDetails ?? null;
    const addSuccessAlert = govtoolProps.addSuccessAlert ?? noop;
    const addErrorAlert = govtoolProps.addErrorAlert ?? noop;
    const addWarningAlert = govtoolProps.addWarningAlert ?? noop;
    const addChangesSavedAlert = govtoolProps.addChangesSavedAlert ?? noop;

    const walletAPIRef = useRef(walletAPI);
    walletAPIRef.current = walletAPI;
    const restoreRunRef = useRef(0);

    const clearStates = () => {
        setUser(null);
    };

    // Session sync: validate or restore the JWT once per wallet transition.
    useEffect(() => {
        // Every transition, or an unmount, makes an earlier run stale: nothing
        // it does after an await may touch the user, the session or the UI.
        const run = ++restoreRunRef.current;
        const isStale = () => run !== restoreRunRef.current;

        if (walletStatus === 'connecting') return;
        if (walletStatus === 'disconnected') {
            if (getDataFromSession('pdfUserJwt') || user) {
                clearStates();
                clearSession();
            }
            return;
        }
        if (!getDataFromSession('pdfUserJwt')) {
            if (user) setUser(null);
            return;
        }
        const ifCurrent =
            (fn) =>
            (...args) => {
                if (!isStale()) fn(...args);
            };
        loginUserToApp({
            wallet: walletAPI,
            setUser: ifCurrent(setUser),
            setOpenUsernameModal: ifCurrent(setOpenUsernameModal),
            trigerSignData: false,
            clearStates: ifCurrent(clearStates),
            addErrorAlert: ifCurrent(addErrorAlert),
            addSuccessAlert: ifCurrent(addSuccessAlert),
            addChangesSavedAlert: ifCurrent(addChangesSavedAlert),
            isStale,
        });
    }, [walletStatus, walletAPI?.stakeKey, walletAPI?.dRepID]);

    useEffect(
        () => () => {
            restoreRunRef.current += 1;
        },
        []
    );

    // The single refresh loop.
    useEffect(() => {
        if (user && user?.user?.govtool_username) {
            const interval = setInterval(async () => {
                const jwt = decodeJWT(); // Get JWT from session
                if (jwt) {
                    const expDate = new Date(jwt?.exp * 1000); // Transfer exp from ms to date
                    const now = new Date();

                    // Check if token is still valid
                    if (expDate <= now) {
                        setUser(null);
                        clearSession();
                        clearInterval(interval); // Clear because user do not exist
                    } else if (expDate - now <= 300000) {
                        // If difference is less then 5 minutes, get new refresh token
                        try {
                            const refreshedTokens = await getRefreshToken(); // Call refreshToken function
                            // Set new JWT in Session
                            saveDataInSession(
                                'pdfUserJwt',
                                refreshedTokens.jwt
                            );
                            // The refresh cookie is shared by every tab, so the
                            // new JWT may belong to another stake key. Without a
                            // ready wallet the session sync checks it later.
                            const liveWallet = walletAPIRef.current;
                            if (
                                liveWallet &&
                                decodeJWT()?.stakeKey !== liveWallet.stakeKey
                            ) {
                                setUser(null);
                                clearSession();
                            }
                        } catch (error) {
                            console.error('Error refreshing token:', error);
                            setUser(null); // Logout user if refresh token fails
                            clearSession();
                        }
                    }
                } else {
                    setUser(null);
                    clearInterval(interval); // Clear interval if there is no token
                }
            }, 60 * 1000); // Every minute

            return () => clearInterval(interval); // Clear interval on component unmount
        }
    }, [user]);

    // The host username is a mirror of the backend one, cleared on logout.
    const username = user?.user?.govtool_username ?? '';
    useEffect(() => {
        govtoolProps.setUsername?.(username);
    }, [username]);

    return (
        <AppContext.Provider
            value={{
                user,
                setUser,
                loading,
                setLoading,
                walletStatus,
                walletAPI,
                locale,
                openUsernameModal,
                setOpenUsernameModal,
                validateMetadata,
                clearStates,
                fetchDRepVotingPowerList,
                getEnactedProposalDetails,
                addSuccessAlert,
                addErrorAlert,
                addWarningAlert,
                addChangesSavedAlert,
                showIdentificationPage,
                setShowIdentificationPage,
                identificationType,
                setIdentificationType,
                epochParams,
            }}
        >
            {children}
        </AppContext.Provider>
    );
}

// For read-only pages, such as the 2025 budget proposals archive: no user, no
// wallet and no session sync, so mounting one never logs a forum user out or
// calls the forum backend.
export function ReadOnlyAppContextProvider({ children, govtoolProps = {} }) {
    const [loading, setLoading] = useState(false);
    return (
        <AppContext.Provider
            value={{
                user: null,
                setUser: noop,
                loading,
                setLoading,
                walletStatus: 'disconnected',
                walletAPI: null,
                locale: govtoolProps.locale ?? 'en',
                openUsernameModal: { open: false, callBackFn: noop },
                setOpenUsernameModal: noop,
                validateMetadata: null,
                clearStates: noop,
                fetchDRepVotingPowerList:
                    govtoolProps.fetchDRepVotingPowerList ?? null,
                getEnactedProposalDetails: null,
                addSuccessAlert: noop,
                addErrorAlert: noop,
                addWarningAlert: noop,
                addChangesSavedAlert: noop,
                showIdentificationPage: false,
                setShowIdentificationPage: noop,
                identificationType: 'wallet',
                setIdentificationType: noop,
                epochParams: govtoolProps.epochParams,
            }}
        >
            {children}
        </AppContext.Provider>
    );
}

export function useAppContext() {
    const context = useContext(AppContext);
    if (context === undefined) {
        throw new Error(
            'useAppContext must be used within a AppContextProvider'
        );
    }

    return context;
}
