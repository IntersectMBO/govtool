import { afterEach, describe, expect, it, vi } from 'vitest';

import {
    activeVersion,
    categorySlug,
    commentThreads,
    displayedPoll,
    filterArchiveItems,
    findCategoryBySlug,
    getArchiveDiscussion,
    getArchiveList,
    resetArchiveListCache,
} from '../budgetArchive';

const item = ({ masterId, name, proposer, type, createdAt, comments }) => ({
    id: Number(masterId),
    attributes: {
        master_id: masterId,
        createdAt,
        prop_comments_number: comments,
        creator: { data: { attributes: { govtool_username: proposer } } },
        bd_psapb: {
            data: {
                attributes: {
                    type_name: {
                        data: { id: type.id, attributes: { type_name: type.name } },
                    },
                },
            },
        },
        bd_proposal_detail: {
            data: { attributes: { proposal_name: name } },
        },
    },
});

const CORE = { id: 1, name: 'Core' };
const RESEARCH = { id: 2, name: 'Research' };

const ITEMS = [
    item({
        masterId: '3',
        name: 'Fund management platform',
        proposer: 'rcircle',
        type: CORE,
        createdAt: '2025-04-01T15:03:14.390Z',
        comments: 12,
    }),
    item({
        masterId: '4',
        name: 'Zero knowledge research',
        proposer: 'alice',
        type: RESEARCH,
        createdAt: '2025-04-03T10:00:00.000Z',
        comments: 0,
    }),
    item({
        masterId: '5',
        name: 'Ada node tooling',
        proposer: 'bob',
        type: CORE,
        createdAt: '2025-04-02T10:00:00.000Z',
        comments: 40,
    }),
];

const jsonResponse = (body, { status = 200, type = 'application/json' } = {}) => ({
    ok: status >= 200 && status < 300,
    status,
    headers: { get: () => type },
    json: async () => body,
});

const ids = (items) => items.map((i) => i.attributes.master_id);

describe('filterArchiveItems', () => {
    it('sorts newest first by default', () => {
        expect(ids(filterArchiveItems(ITEMS))).toEqual(['4', '5', '3']);
    });

    it('keeps only the chosen categories', () => {
        expect(
            ids(filterArchiveItems(ITEMS, { categoryIds: [CORE.id] }))
        ).toEqual(['5', '3']);
    });

    it('treats an empty category list as every category', () => {
        expect(filterArchiveItems(ITEMS, { categoryIds: [] })).toHaveLength(
            3
        );
    });

    it('searches the name and the proposer, ignoring case', () => {
        expect(
            ids(filterArchiveItems(ITEMS, { searchText: '  FUND ' }))
        ).toEqual(['3']);
        expect(ids(filterArchiveItems(ITEMS, { searchText: 'Alice' }))).toEqual(
            ['4']
        );
        expect(filterArchiveItems(ITEMS, { searchText: 'nothing' })).toEqual(
            []
        );
    });

    it('combines search and category', () => {
        expect(
            ids(
                filterArchiveItems(ITEMS, {
                    searchText: 'research',
                    categoryIds: [CORE.id],
                })
            )
        ).toEqual([]);
    });

    it.each([
        ['oldest', ['3', '5', '4']],
        ['most-comments', ['5', '3', '4']],
        ['least-comments', ['4', '3', '5']],
        ['name-asc', ['5', '3', '4']],
        ['name-desc', ['4', '3', '5']],
        ['proposer-asc', ['4', '5', '3']],
        ['proposer-desc', ['3', '5', '4']],
    ])('sorts by %s', (sortId, expected) => {
        expect(ids(filterArchiveItems(ITEMS, { sortId }))).toEqual(expected);
    });

    it('orders by the proposal date, not the last edit', () => {
        const edited = {
            ...ITEMS[0],
            attributes: {
                ...ITEMS[0].attributes,
                createdAt: '2025-06-01T00:00:00.000Z',
                master_proposal_created_at: '2025-04-01T15:03:14.390Z',
            },
        };
        expect(ids(filterArchiveItems([edited, ...ITEMS.slice(1)]))).toEqual(
            ['4', '5', '3']
        );
    });

    it('does not reorder the input', () => {
        const input = [...ITEMS];
        filterArchiveItems(input, { sortId: 'oldest' });
        expect(input).toEqual(ITEMS);
    });
});

describe('categories', () => {
    const categories = [
        { id: 4, attributes: { type_name: 'Marketing & Innovation' } },
        { id: 5, attributes: { type_name: 'None of these' } },
    ];

    it('slugs a category as the old routes and test ids did', () => {
        expect(categorySlug('Marketing & Innovation')).toBe(
            'marketing-&-innovation'
        );
        expect(categorySlug('None of these')).toBe('no-category');
    });

    it('finds a category from a route slug, encoded or not', () => {
        expect(findCategoryBySlug(categories, 'no-category')?.id).toBe(5);
        expect(
            findCategoryBySlug(categories, 'marketing-%26-innovation')?.id
        ).toBe(4);
        expect(findCategoryBySlug(categories, 'unknown')).toBeNull();
    });
});

describe('getArchiveList', () => {
    afterEach(() => resetArchiveListCache());

    it('fetches the static list once per session', async () => {
        const fetchImpl = vi.fn(async () => jsonResponse({ items: [] }));
        await getArchiveList(fetchImpl);
        await getArchiveList(fetchImpl);
        expect(fetchImpl).toHaveBeenCalledTimes(1);
        expect(fetchImpl).toHaveBeenCalledWith(
            '/budget-proposals-2025/list.json'
        );
    });

    it('fetches again after a failure', async () => {
        const fetchImpl = vi
            .fn()
            .mockResolvedValueOnce(jsonResponse(null, { status: 503 }))
            .mockResolvedValueOnce(jsonResponse({ items: [] }));
        await expect(getArchiveList(fetchImpl)).rejects.toMatchObject({
            status: 503,
        });
        await expect(getArchiveList(fetchImpl)).resolves.toEqual({
            items: [],
        });
    });
});

describe('getArchiveDiscussion', () => {
    it('fetches the discussion file for a master id', async () => {
        const fetchImpl = vi.fn(async () => jsonResponse({ master_id: '3' }));
        await expect(getArchiveDiscussion('3', fetchImpl)).resolves.toEqual({
            master_id: '3',
        });
        expect(fetchImpl).toHaveBeenCalledWith('/budget-proposals-2025/3.json');
    });

    it('refuses a non-numeric id without a request', async () => {
        const fetchImpl = vi.fn();
        await expect(
            getArchiveDiscussion('../list', fetchImpl)
        ).rejects.toMatchObject({ status: 404 });
        expect(fetchImpl).not.toHaveBeenCalled();
    });

    it('reads an SPA fallback page as not found', async () => {
        const fetchImpl = vi.fn(async () =>
            jsonResponse('<html></html>', { type: 'text/html' })
        );
        await expect(
            getArchiveDiscussion('999', fetchImpl)
        ).rejects.toMatchObject({ status: 404 });
    });
});

describe('activeVersion and displayedPoll', () => {
    it('picks the active version, else the first', () => {
        const old = { id: 1, attributes: { is_active: false } };
        const live = { id: 2, attributes: { is_active: true } };
        expect(activeVersion([old, live])).toBe(live);
        expect(activeVersion([old])).toBe(old);
        expect(activeVersion([])).toBeNull();
    });

    it('shows the newest active poll, else the newest', () => {
        const poll = (id, createdAt, active) => ({
            id,
            attributes: { createdAt, is_poll_active: active },
        });
        const older = poll(1, '2025-04-01T00:00:00Z', true);
        const newer = poll(2, '2025-04-02T00:00:00Z', true);
        const closed = poll(3, '2025-04-03T00:00:00Z', false);
        expect(displayedPoll([older, newer, closed])).toBe(newer);
        expect(displayedPoll([older, closed].map((p) => ({
            ...p,
            attributes: { ...p.attributes, is_poll_active: false },
        })))?.id).toBe(3);
        expect(displayedPoll([])).toBeNull();
    });
});

describe('commentThreads', () => {
    const comment = (id, parent, createdAt) => ({
        id,
        attributes: { comment_parent_id: parent, createdAt },
    });
    const COMMENTS = [
        comment(1, null, '2025-04-01T00:00:00Z'),
        comment(2, '1', '2025-04-03T00:00:00Z'),
        comment(3, null, '2025-04-02T00:00:00Z'),
        comment(4, '1', '2025-04-02T12:00:00Z'),
    ];

    it('nests replies under their parent, oldest reply first', () => {
        const threads = commentThreads(COMMENTS, 'asc');
        expect(threads.map((t) => t.comment.id)).toEqual([1, 3]);
        expect(threads[0].replies.map((r) => r.id)).toEqual([4, 2]);
        expect(threads[1].replies).toEqual([]);
    });

    it('orders top-level comments newest first by default', () => {
        expect(commentThreads(COMMENTS).map((t) => t.comment.id)).toEqual([
            3, 1,
        ]);
    });
});
