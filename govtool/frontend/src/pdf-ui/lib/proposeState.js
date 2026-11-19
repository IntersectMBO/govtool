import { useEffect, useMemo, useRef } from 'react';

import { useAppContext } from '../context/context';
import { loginUserToApp } from './helpers';

/**
 * Where a visitor stands on proposing: no wallet, a wallet not yet signed in
 * to the forum, signed in without a display name, or ready.
 */
export const proposeStatus = (walletAPI, user) => {
    if (!walletAPI) return 'disconnected';
    if (!user) return 'unverified';
    if (!user?.user?.govtool_username) return 'noUsername';
    return 'ready';
};

/**
 * Reports the propose status, with the actions that move it on, to GovTool's
 * page header (onProposeStateChange), once per change and null on unmount.
 * openProposeDialog runs once a propose sign-in succeeds.
 */
export const useReportProposeState = (openProposeDialog) => {
    const {
        user,
        walletAPI,
        setUser,
        setOpenUsernameModal,
        clearStates,
        addSuccessAlert,
        addErrorAlert,
        addChangesSavedAlert,
        onProposeStateChange,
    } = useAppContext();
    const status = proposeStatus(walletAPI, user);

    const login = (extra) =>
        loginUserToApp({
            wallet: walletAPI,
            setUser,
            setOpenUsernameModal,
            clearStates,
            addErrorAlert,
            addSuccessAlert,
            addChangesSavedAlert,
            ...extra,
        });

    // The header keeps one set of actions; they call the latest closures.
    const latest = useRef({});
    latest.current = {
        verify: () => login({ trigerSignData: true }),
        createUsername: () =>
            setOpenUsernameModal({ open: true, callBackFn: () => {} }),
        propose: () => login({ callBackFn: () => openProposeDialog() }),
    };
    const actions = useMemo(
        () => ({
            verify: () => latest.current.verify(),
            createUsername: () => latest.current.createUsername(),
            propose: () => latest.current.propose(),
        }),
        []
    );

    useEffect(() => {
        onProposeStateChange?.({ status, ...actions });
    }, [status, onProposeStateChange, actions]);

    useEffect(
        () => () => onProposeStateChange?.(null),
        [onProposeStateChange]
    );

    return status;
};
