import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest';

import '@/i18n';

const ctx = vi.hoisted(() => ({ user: null, walletAPI: null }));
const record = vi.hoisted(() => ({ governanceAction: undefined }));

vi.mock('../../../context/context', () => ({
    useAppContext: () => ({
        user: ctx.user,
        walletAPI: ctx.walletAPI,
        setLoading: () => {},
        setOpenUsernameModal: () => {},
        setUser: () => {},
        clearStates: () => {},
        addSuccessAlert: () => {},
        addErrorAlert: () => {},
        addChangesSavedAlert: () => {},
        epochParams: null,
    }),
}));

vi.mock('../../../lib/api', () => ({
    createComment: vi.fn(),
    createProposalLikeOrDislike: vi.fn(),
    deleteProposal: vi.fn(),
    getComments: vi.fn(),
    getSingleProposal: vi.fn(),
    getUserProposalVote: vi.fn(),
    updateProposalLikesOrDislikes: vi.fn(),
    createPoll: vi.fn(),
    getPolls: vi.fn(),
}));

// The page's child components have their own tests; here they are stubs.
vi.mock('../../../components', () => ({
    CommentCard: ({ comment }) => (
        <div data-testid={`stub-comment-${comment.id}`} />
    ),
    EditProposalDialog: () => <div data-testid='stub-edit-dialog' />,
    Poll: () => null,
    ReviewVersions: () => null,
    ProposalSubmissionDialog: () => null,
    DeleteProposalModal: () => null,
}));

vi.mock('../../../components/ProposalOwnModal', () => ({ default: () => null }));

vi.mock('@/hooks/queries/useGetGovernanceActionRecordQuery', () => ({
    useGetGovernanceActionRecordQuery: () => record,
}));

vi.mock('@/context/snackbar', () => ({
    useSnackbar: () => ({ addSuccessAlert: () => {} }),
}));

import {
    getComments,
    getPolls,
    getSingleProposal,
    getUserProposalVote,
} from '../../../lib/api';
import SingleGovernanceAction from './index';

const treasury = (overrides = {}) => ({
    id: 7,
    attributes: {
        user_id: 42,
        prop_likes: 3,
        prop_dislikes: 1,
        prop_comments_number: 150,
        user_govtool_username: 'kristin',
        createdAt: '2026-09-11T00:00:00.000Z',
        content: {
            attributes: {
                prop_name: 'Fund a developer grant round',
                prop_abstract: 'A short abstract.',
                prop_motivation: 'Why.',
                prop_rationale: 'Because.',
                prop_submitted: false,
                is_draft: false,
                gov_action_type_id: 2,
                createdAt: '2026-09-12T00:00:00.000Z',
                gov_action_type: {
                    attributes: { gov_action_type_name: 'Treasury requests' },
                },
                proposal_withdrawals: [
                    {
                        prop_receiving_address: 'stake1uabc',
                        prop_amount: 1000,
                    },
                ],
                proposal_links: [
                    { prop_link: 'https://example.com', prop_link_text: 'Plan' },
                ],
                ...overrides,
            },
        },
    },
});

const renderPage = async (proposal) => {
    getSingleProposal.mockResolvedValue(proposal);
    render(
        <MemoryRouter>
            <SingleGovernanceAction id='7' />
        </MemoryRouter>
    );
    return screen.findByTestId('title-content');
};

describe('SingleGovernanceAction', () => {
    beforeEach(() => {
        getComments.mockResolvedValue({ comments: [], pgCount: 0 });
        getPolls.mockResolvedValue({ polls: [] });
        getUserProposalVote.mockResolvedValue(null);
    });

    afterEach(() => {
        ctx.user = null;
        ctx.walletAPI = null;
        record.governanceAction = undefined;
        vi.clearAllMocks();
    });

    it('shows the proposal, its type details and links once each', async () => {
        expect(await renderPage(treasury())).toHaveTextContent(
            'Fund a developer grant round'
        );
        expect(
            screen.getByTestId('governance-action-type-content')
        ).toHaveTextContent('Treasury requests');
        expect(screen.getByTestId('abstract-content')).toHaveTextContent(
            'A short abstract.'
        );
        expect(screen.getByTestId('motivation-content')).toBeInTheDocument();
        expect(screen.getByTestId('rationale-content')).toBeInTheDocument();
        expect(
            screen.getByTestId('receiving-address-0-content')
        ).toHaveTextContent('stake1uabc');
        expect(screen.getByTestId('amount-0-content').textContent).toBe(
            '₳ 1000'
        );
        expect(screen.getByTestId('link-0-text-content')).toHaveTextContent(
            'Plan'
        );
        expect(screen.getByTestId('link-0-url-content')).toHaveTextContent(
            'https://example.com'
        );
        // The counts keep the old badges' 99+ cap.
        expect(screen.getByTestId('comment-count')).toHaveTextContent('99+');
        expect(screen.getByTestId('like-count')).toHaveTextContent('3');
        expect(screen.getByTestId('dislike-count')).toHaveTextContent('1');
        expect(screen.getByTestId('review-version')).toBeInTheDocument();
        expect(screen.getByRole('heading', { level: 1 })).toHaveTextContent(
            'Fund a developer grant round'
        );
    });

    it('asks a signed-out visitor to connect and disables sentiment', async () => {
        await renderPage(treasury());
        expect(screen.getByTestId('like-button')).toBeDisabled();
        expect(screen.getByTestId('dislike-button')).toBeDisabled();
        expect(screen.getAllByTestId('connect-wallet-link').length).toBeGreaterThan(0);
        expect(screen.queryByTestId('menu-button')).toBeNull();
        expect(screen.queryByTestId('submit-as-GA-button')).toBeNull();
        expect(screen.queryByTestId('add-poll-card')).toBeNull();
        expect(
            screen.getByRole('textbox', { name: 'Add a comment' })
        ).toBeInTheDocument();
    });

    it('gives the author the menu, submission and a poll', async () => {
        ctx.user = { user: { id: 42, govtool_username: 'kristin' } };
        ctx.walletAPI = { address: 'addr1' };
        await renderPage(treasury());
        expect(screen.getByTestId('submit-as-GA-button')).toBeInTheDocument();
        expect(await screen.findByTestId('add-poll-card')).toBeInTheDocument();
        fireEvent.click(screen.getByTestId('menu-button'));
        expect(screen.getByTestId('edit-proposal')).toBeInTheDocument();
        expect(screen.getByTestId('delete-proposal')).toBeInTheDocument();
    });

    it('confirms beside the share button that the link was copied', async () => {
        const writeText = vi.fn().mockResolvedValue(undefined);
        Object.assign(navigator, { clipboard: { writeText } });
        await renderPage(treasury());
        expect(screen.queryByTestId('copy-link-text')).toBeNull();
        fireEvent.click(screen.getByTestId('share-button'));
        await act(async () => {
            fireEvent.click(screen.getByTestId('copy-link'));
        });
        expect(writeText).toHaveBeenCalledWith(
            `${window.location.origin}/proposals/7`
        );
        expect(screen.getByTestId('copy-link-text')).toHaveTextContent(
            'Link copied'
        );
    });

    it('links a submitted proposal to its vote', async () => {
        await renderPage(
            treasury({
                prop_submitted: true,
                prop_submission_tx_hash: 'abc',
                prop_submission_date: '2026-09-20T00:00:00.000Z',
            })
        );
        expect(screen.getByTestId('review-and-vote-link')).toBeInTheDocument();
        expect(screen.queryByTestId('comment-input')).toBeNull();
        expect(screen.getByTestId('proposal-status')).toHaveTextContent(
            'Submitted for vote'
        );
    });

    it('reports a decided vote and links to its outcome', async () => {
        record.governanceAction = {
            epoch_no: 600,
            status: {
                ratified_epoch: null,
                enacted_epoch: null,
                dropped_epoch: null,
                expired_epoch: 610,
            },
            status_times: { expired_time: '2026-10-01T00:00:00.000Z' },
        };
        await renderPage(
            treasury({
                prop_submitted: true,
                prop_submission_tx_hash: 'abc',
                prop_submission_date: '2026-09-20T00:00:00.000Z',
            })
        );
        expect(screen.getByTestId('vote-ended-date')).toHaveTextContent(
            'Epoch 610'
        );
        expect(screen.getByTestId('vote-outcome')).toHaveTextContent(
            'This action was expired.'
        );
        expect(screen.getByTestId('see-outcome-link')).toBeInTheDocument();
        expect(screen.queryByTestId('review-and-vote-link')).toBeNull();
    });

    it("shows a constitution's URL and guardrails script", async () => {
        await renderPage(
            treasury({
                gov_action_type_id: 3,
                proposal_withdrawals: [],
                proposal_constitution_content: {
                    data: {
                        attributes: {
                            prop_constitution_url: 'https://c.example',
                            prop_have_guardrails_script: true,
                            prop_guardrails_script_url: 'https://g.example',
                            prop_guardrails_script_hash: 'abc123',
                        },
                    },
                },
            })
        );
        expect(
            screen.getByTestId('new-constitution-url-content')
        ).toHaveTextContent('https://c.example');
        expect(
            screen.getByTestId('guardrails-script-url-content')
        ).toHaveTextContent('https://g.example');
        expect(
            screen.getByTestId('guardrails-script-hash-content')
        ).toHaveTextContent('abc123');
        expect(screen.queryByTestId('amount-0-content')).toBeNull();
    });

    it("shows a hard fork's versions and previous action", async () => {
        await renderPage(
            treasury({
                gov_action_type_id: 6,
                proposal_withdrawals: [],
                proposal_hard_fork_content: {
                    data: {
                        attributes: {
                            previous_ga_hash: 'ff00',
                            previous_ga_id: '0',
                            major: '11',
                            minor: '2',
                        },
                    },
                },
            })
        );
        expect(screen.getByTestId('major-version-content')).toHaveTextContent(
            '11'
        );
        expect(screen.getByTestId('minor-version-content')).toHaveTextContent(
            '2'
        );
        expect(
            screen.getByTestId('previous-gov-action-hash-content')
        ).toHaveTextContent('ff00');
        expect(
            screen.getByTestId('previous-gov-action-id-content')
        ).toHaveTextContent('0');
    });

    it('lists the comments and pages through them', async () => {
        getComments.mockResolvedValue({
            comments: [{ id: 1 }, { id: 2 }],
            pgCount: 2,
        });
        await renderPage(treasury());
        expect(await screen.findByTestId('stub-comment-1')).toBeInTheDocument();
        expect(screen.getByTestId('stub-comment-2')).toBeInTheDocument();
        expect(
            screen.getByRole('button', { name: 'Load more comments' })
        ).toBeInTheDocument();
        fireEvent.click(screen.getByTestId('sort-comments'));
        await waitFor(() =>
            expect(getComments).toHaveBeenLastCalledWith(
                expect.stringContaining('sort[createdAt]=asc')
            )
        );
    });

    // As before the redesign, only an abstract over 500 characters folds;
    // a short abstract leaves the rest open with Show less.
    it('folds a long abstract behind Read more', async () => {
        await renderPage(treasury({ prop_abstract: 'a'.repeat(600) }));
        expect(screen.queryByTestId('motivation-content')).toBeNull();
        expect(
            screen.getByTestId('abstract-content').textContent.length
        ).toBeLessThan(600);
        fireEvent.click(screen.getByRole('button', { name: 'Read more' }));
        await waitFor(() =>
            expect(screen.getByTestId('motivation-content')).toBeInTheDocument()
        );
        expect(
            screen.getByRole('button', { name: 'Show less' })
        ).toBeInTheDocument();
    });
});
