import { describe, expect, it } from 'vitest';

import {
    cappedCount,
    proposalActionKind,
    proposalStatus,
    sentimentButton,
    typeDetailsKind,
} from './detailsState';

describe('proposalActionKind', () => {
    it('lets the signed-in author submit an unsubmitted proposal', () => {
        expect(
            proposalActionKind({
                submitted: false,
                hasUser: true,
                isAuthor: true,
                voteEnded: false,
            })
        ).toBe('submit');
    });

    it('asks everyone else to comment', () => {
        expect(
            proposalActionKind({ submitted: false, hasUser: true, isAuthor: false })
        ).toBe('discuss');
        expect(
            proposalActionKind({ submitted: false, hasUser: false, isAuthor: true })
        ).toBe('discuss');
    });

    it('needs submitted to be exactly false before offering submission', () => {
        expect(
            proposalActionKind({
                submitted: undefined,
                hasUser: true,
                isAuthor: true,
            })
        ).toBe('discuss');
    });

    it('links a submitted proposal to its vote, then to its outcome', () => {
        expect(
            proposalActionKind({
                submitted: true,
                hasUser: true,
                isAuthor: true,
                voteEnded: false,
            })
        ).toBe('vote');
        expect(
            proposalActionKind({ submitted: true, hasUser: false, voteEnded: true })
        ).toBe('outcome');
    });
});

describe('proposalStatus', () => {
    it('words the status as the proposals list does', () => {
        expect(proposalStatus(false)).toEqual({ label: 'Active', tone: 'success' });
        expect(proposalStatus(true)).toEqual({
            label: 'Submitted for vote',
            tone: 'primary',
        });
    });
});

describe('cappedCount', () => {
    it('caps at 99+ and shows zero for a missing count', () => {
        expect(cappedCount(undefined)).toBe('0');
        expect(cappedCount(0)).toBe('0');
        expect(cappedCount(99)).toBe('99');
        expect(cappedCount(100)).toBe('99+');
    });
});

describe('typeDetailsKind', () => {
    it('shows withdrawals only when the treasury proposal has some', () => {
        expect(
            typeDetailsKind({
                gov_action_type_id: 2,
                proposal_withdrawals: [{ prop_amount: 1 }],
            })
        ).toBe('withdrawals');
        expect(
            typeDetailsKind({ gov_action_type_id: '2', proposal_withdrawals: [] })
        ).toBeNull();
    });

    it('shows the constitution only with its content', () => {
        expect(
            typeDetailsKind({
                gov_action_type_id: 3,
                proposal_constitution_content: { data: { attributes: {} } },
            })
        ).toBe('constitution');
        expect(
            typeDetailsKind({
                gov_action_type_id: 3,
                proposal_constitution_content: { data: null },
            })
        ).toBeNull();
    });

    it('shows hard fork details and nothing for other types', () => {
        expect(typeDetailsKind({ gov_action_type_id: 6 })).toBe('hardFork');
        expect(typeDetailsKind({ gov_action_type_id: 1 })).toBeNull();
        expect(typeDetailsKind(undefined)).toBeNull();
    });
});

describe('sentimentButton', () => {
    const user = { user: { id: 1, govtool_username: 'kristin' } };
    const base = {
        like: true,
        submitted: false,
        hasWallet: true,
        user,
        isAuthor: false,
        vote: null,
    };

    it('lets a signed-in reader like a proposal', () => {
        expect(sentimentButton(base)).toEqual({
            tooltip: 'Like this proposal\n\nClick to like this proposal',
            disabled: false,
            action: 'vote',
            active: false,
        });
    });

    it('disables both buttons once the proposal is submitted', () => {
        const state = sentimentButton({ ...base, like: false, submitted: true });
        expect(state.tooltip).toBe(
            "Proposal Submitted\n\nYou can't dislike this proposal"
        );
        expect(state.disabled).toBe(true);
        expect(state.action).toBeNull();
    });

    it('asks for a wallet, then for verification', () => {
        expect(sentimentButton({ ...base, hasWallet: false })).toMatchObject({
            tooltip: 'Connect wallet to like this proposal',
            disabled: true,
        });
        expect(
            sentimentButton({ ...base, user: null, isAuthor: true })
        ).toMatchObject({
            tooltip: 'Verify yourself to like this proposal',
            disabled: false,
            action: 'vote',
            active: false,
        });
    });

    it('asks for a display name before recording a vote', () => {
        expect(
            sentimentButton({ ...base, user: { user: { id: 1 } } }).action
        ).toBe('username');
    });

    it("keeps authors from voting on their own proposal", () => {
        expect(sentimentButton({ ...base, isAuthor: true })).toMatchObject({
            tooltip: "You can't like your proposal",
            disabled: true,
            action: null,
        });
    });

    it("marks the user's own vote and lets them switch to the other", () => {
        const vote = { id: 9, attributes: { vote_result: false } };
        expect(
            sentimentButton({ ...base, like: false, vote })
        ).toMatchObject({
            tooltip: 'You already disliked this proposal',
            disabled: true,
            active: true,
        });
        expect(sentimentButton({ ...base, vote })).toMatchObject({
            tooltip: 'Like this proposal\n\nClick to like this proposal',
            disabled: false,
            action: 'vote',
            active: false,
        });
    });

    it('does nothing for a vote whose result is null', () => {
        const vote = { id: 9, attributes: { vote_result: null } };
        expect(sentimentButton({ ...base, vote }).action).toBeNull();
    });
});
