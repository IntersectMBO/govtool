import { renderHook } from '@testing-library/react';
import { beforeEach, describe, expect, it, vi } from 'vitest';

import { proposeStatus, useReportProposeState } from '../proposeState';

const mocks = vi.hoisted(() => ({
    context: {},
    login: vi.fn(),
}));

vi.mock('../../context/context', () => ({
    useAppContext: () => mocks.context,
}));

vi.mock('../helpers', () => ({
    loginUserToApp: (args) => mocks.login(args),
}));

describe('proposeStatus', () => {
    it('follows the wallet, the forum sign-in and the display name', () => {
        expect(proposeStatus(null, null)).toBe('disconnected');
        expect(proposeStatus({ address: 'a' }, null)).toBe('unverified');
        expect(proposeStatus({ address: 'a' }, { user: {} })).toBe('noUsername');
        expect(
            proposeStatus({ address: 'a' }, { user: { govtool_username: 'kd' } })
        ).toBe('ready');
    });
});

describe('useReportProposeState', () => {
    beforeEach(() => {
        mocks.login.mockReset();
        mocks.context = {
            walletAPI: { address: 'a' },
            user: null,
            setUser: vi.fn(),
            setOpenUsernameModal: vi.fn(),
            clearStates: vi.fn(),
            onProposeStateChange: vi.fn(),
        };
    });

    it('reports each status once, and nothing once unmounted', () => {
        const report = mocks.context.onProposeStateChange;
        const { rerender, unmount } = renderHook(() =>
            useReportProposeState(() => {})
        );
        rerender();
        expect(report).toHaveBeenCalledTimes(1);
        expect(report.mock.calls[0][0].status).toBe('unverified');

        mocks.context = { ...mocks.context, user: { user: { govtool_username: 'kd' } } };
        rerender();
        expect(report).toHaveBeenLastCalledWith(
            expect.objectContaining({ status: 'ready' })
        );
        unmount();
        expect(report).toHaveBeenLastCalledWith(null);
    });

    it('signs in to verify, and opens the propose dialog after signing in', () => {
        const openDialog = vi.fn();
        const { result } = renderHook(() => useReportProposeState(openDialog));
        const state = mocks.context.onProposeStateChange.mock.calls[0][0];

        state.verify();
        expect(mocks.login).toHaveBeenLastCalledWith(
            expect.objectContaining({ trigerSignData: true })
        );

        state.propose();
        mocks.login.mock.lastCall[0].callBackFn();
        expect(openDialog).toHaveBeenCalled();

        state.createUsername();
        expect(mocks.context.setOpenUsernameModal).toHaveBeenCalledWith(
            expect.objectContaining({ open: true })
        );
        expect(result.current).toBe('unverified');
    });
});
