import { act, fireEvent, render, screen, waitFor } from '@testing-library/react';
import { beforeEach, describe, expect, it, vi } from 'vitest';

import ProposalRows from './index';

const api = vi.hoisted(() => ({ calls: [], resolvers: [] }));

vi.mock('../../lib/api', () => ({
    getProposals: (query) =>
        new Promise((resolve) => {
            api.calls.push(query);
            api.resolvers.push(resolve);
        }),
}));

vi.mock('../ProposalRow', () => ({
    default: ({ proposal }) => (
        <div data-testid='row'>{proposal.attributes.content.attributes.prop_name}</div>
    ),
}));

const row = (id, name) => ({
    id,
    attributes: { content: { attributes: { prop_name: name } } },
});

const answer = (index, names, total = names.length) =>
    act(async () => {
        api.resolvers[index]({
            proposals: names.map((name, i) => row(`${index}-${i}`, name)),
            total,
        });
    });

const props = {
    search: '',
    typeIds: [],
    statuses: ['active'],
    sort: { fieldId: 'createdAt', type: 'DESC' },
};

beforeEach(() => {
    api.calls = [];
    api.resolvers = [];
    vi.spyOn(window, 'scrollTo').mockImplementation(() => {});
});

describe('ProposalRows', () => {
    it('keeps the newest filter when an older answer arrives last', async () => {
        const { rerender } = render(<ProposalRows {...props} search='a' />);
        rerender(<ProposalRows {...props} search='b' />);
        await answer(1, ['Only b']);
        await answer(0, ['Stale a']);
        expect(screen.getAllByTestId('row').map((el) => el.textContent)).toEqual(['Only b']);
    });

    it('goes back to the first page when a filter changes', async () => {
        const { rerender } = render(<ProposalRows {...props} />);
        await answer(0, Array.from({ length: 10 }, (_, i) => `P${i}`), 25);
        fireEvent.click(screen.getByRole('button', { name: '2' }));
        await waitFor(() => expect(api.calls.at(-1)).toContain('pagination[page]=2'));

        rerender(<ProposalRows {...props} typeIds={[3]} />);
        await waitFor(() =>
            expect(api.calls.at(-1)).toContain('[gov_action_type_id][$in][0]=3')
        );
        expect(api.calls.at(-1)).toContain('pagination[page]=1');
    });

    it('stays on the first page when a filter is undone', async () => {
        const { rerender } = render(<ProposalRows {...props} />);
        await answer(0, Array.from({ length: 10 }, (_, i) => `P${i}`), 25);
        fireEvent.click(screen.getByRole('button', { name: '2' }));
        await waitFor(() => expect(api.calls.at(-1)).toContain('pagination[page]=2'));

        rerender(<ProposalRows {...props} typeIds={[3]} />);
        await waitFor(() => expect(api.calls.at(-1)).toContain('[$in][0]=3'));
        rerender(<ProposalRows {...props} />);
        await waitFor(() => expect(api.calls.at(-1)).not.toContain('[$in]'));
        expect(api.calls.at(-1)).toContain('pagination[page]=1');
    });

    it('moves back to the last page when the current one comes back empty', async () => {
        render(<ProposalRows {...props} />);
        await answer(0, Array.from({ length: 10 }, (_, i) => `P${i}`), 25);
        fireEvent.click(screen.getByRole('button', { name: '3' }));
        await waitFor(() => expect(api.calls.at(-1)).toContain('pagination[page]=3'));
        // Five proposals went away meanwhile: 20 left, two pages.
        await answer(api.resolvers.length - 1, [], 20);
        await waitFor(() => expect(api.calls.at(-1)).toContain('pagination[page]=2'));
        expect(screen.queryByTestId('proposals-empty-state')).toBeNull();
    });

    it('says so when the forum cannot be reached', async () => {
        render(<ProposalRows {...props} />);
        await act(async () => api.resolvers[0](new Error('Network Error')));
        expect(screen.getByTestId('proposals-empty-state')).toHaveTextContent(
            'Proposals could not be loaded'
        );
    });
});
