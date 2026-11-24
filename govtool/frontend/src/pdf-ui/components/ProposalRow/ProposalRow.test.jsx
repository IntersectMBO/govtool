import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';

import '@/i18n';
import { describe, expect, it, vi } from 'vitest';

import ProposalRow from './index';

const ctx = vi.hoisted(() => ({ user: null }));

vi.mock('../../context/context', () => ({
    useAppContext: () => ({ user: ctx.user, addSuccessAlert: () => {} }),
}));

vi.mock('../EditProposalDialog', () => ({ default: () => null }));

vi.mock('@/context/snackbar', () => ({
    useSnackbar: () => ({ addSuccessAlert: () => {} }),
}));

const proposal = {
    id: 7,
    attributes: {
        prop_likes: 3,
        prop_dislikes: 1,
        prop_comments_number: 1,
        user_govtool_username: 'kristin',
        user_id: 42,
        createdAt: '2026-09-11T00:00:00.000Z',
        content: {
            attributes: {
                prop_name: 'Kenya Retirement Edition',
                prop_abstract: '**Retire** for [less](https://x.y).',
                prop_submitted: false,
                gov_action_type: {
                    attributes: { gov_action_type_name: 'Info Action' },
                },
            },
        },
    },
};

const renderRow = () =>
    render(
        <MemoryRouter>
            <ProposalRow proposal={proposal} />
        </MemoryRouter>
    );

describe('ProposalRow', () => {
    it('titles the row with a heading that opens the proposal', () => {
        renderRow();
        const heading = screen.getByRole('heading', {
            level: 3,
            name: 'Kenya Retirement Edition',
        });
        expect(heading.querySelector('a')).toHaveAttribute(
            'href',
            '/proposals/7'
        );
    });

    it("names each row's actions after its proposal", () => {
        ctx.user = { user: { id: 42 } };
        renderRow();
        // The shared share dialog, worded for a proposal.
        expect(screen.getByTestId('proposal-7-share-button')).toHaveAccessibleName(
            'Share proposal'
        );
        expect(
            screen.getByRole('button', { name: 'Edit Kenya Retirement Edition' })
        ).toBeInTheDocument();
        ctx.user = null;
    });

    it('shows the abstract as plain text, the comments and the support', () => {
        renderRow();
        expect(screen.getByTestId('proposal-7-abstract-content')).toHaveTextContent(
            'Retire for less.'
        );
        expect(screen.getByTestId('proposal-7-comment-count')).toHaveTextContent(
            '1 comment'
        );
        expect(screen.getByTestId('proposal-7-support')).toHaveTextContent('75%');
        expect(screen.getByTestId('governance-action-type')).toHaveTextContent(
            'Info Action'
        );
    });
});
