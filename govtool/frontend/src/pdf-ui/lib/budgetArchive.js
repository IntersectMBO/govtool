// The 2025 budget proposals, read-only, from the static files
// scripts/split-bd-archive.mjs writes under public/budget-proposals-2025.
// Nothing here calls a backend.

export const ARCHIVE_BASE_PATH = '/budget-proposals-2025';

// The category the forum labelled "None of these" is shown as "No category".
export const NO_CATEGORY_NAME = 'None of these';

export const SORT_OPTIONS = [
    { id: 'newest', title: 'Newest' },
    { id: 'oldest', title: 'Oldest' },
    { id: 'most-comments', title: 'Most comments' },
    { id: 'least-comments', title: 'Least comments' },
    { id: 'name-asc', title: 'Name A-Z' },
    { id: 'name-desc', title: 'Name Z-A' },
    { id: 'proposer-asc', title: 'Proposer A-Z' },
    { id: 'proposer-desc', title: 'Proposer Z-A' },
];

const fetchJson = async (path, fetchImpl) => {
    const response = await fetchImpl(`${ARCHIVE_BASE_PATH}/${path}`);
    // A server with an SPA fallback (the Vite dev server) answers a missing
    // file with index.html; that is a 404 here too.
    const isJson = `${response.headers?.get('content-type') ?? ''}`.includes(
        'json'
    );
    if (!response.ok || !isJson) {
        const status = response.ok ? 404 : response.status;
        const error = new Error(
            `Budget proposals archive: ${path} answered ${status}`
        );
        error.status = status;
        throw error;
    }
    return response.json();
};

// The list never changes, so one fetch serves every visit in a session.
let listPromise = null;

export const getArchiveList = (fetchImpl = fetch) => {
    if (!listPromise) {
        listPromise = fetchJson('list.json', fetchImpl).catch((error) => {
            listPromise = null;
            throw error;
        });
    }
    return listPromise;
};

export const resetArchiveListCache = () => {
    listPromise = null;
};

// Master ids are numeric; anything else is not in the archive.
export const isArchiveId = (id) => /^\d+$/.test(`${id ?? ''}`);

export const getArchiveDiscussion = async (masterId, fetchImpl = fetch) => {
    if (!isArchiveId(masterId)) {
        const error = new Error(`Not a budget proposal id: ${masterId}`);
        error.status = 404;
        throw error;
    }
    return fetchJson(`${masterId}.json`, fetchImpl);
};

export const categoryName = (item) =>
    item?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.attributes
        ?.type_name ?? '';

export const categoryId = (item) =>
    item?.attributes?.bd_psapb?.data?.attributes?.type_name?.data?.id ?? null;

export const categoryLabel = (name) =>
    name === NO_CATEGORY_NAME ? 'No category' : name;

// The slug a category route carries, as the old test ids spelled it.
export const categorySlug = (name) =>
    name === NO_CATEGORY_NAME
        ? 'no-category'
        : `${name ?? ''}`.trim().replace(/\s+/g, '-').toLowerCase();

export const findCategoryBySlug = (categories, slug) =>
    categories?.find(
        (category) =>
            categorySlug(category?.attributes?.type_name) ===
            decodeURIComponent(`${slug ?? ''}`).toLowerCase()
    ) ?? null;

const proposalName = (item) =>
    item?.attributes?.bd_proposal_detail?.data?.attributes?.proposal_name ??
    '';

const proposer = (item) =>
    item?.attributes?.creator?.data?.attributes?.govtool_username ?? '';

// The date the card shows: when the proposal was first made, not its last
// edit.
const proposedAt = (item) =>
    item?.attributes?.master_proposal_created_at ??
    item?.attributes?.createdAt ??
    '';

const compareText = (a, b) =>
    a.localeCompare(b, undefined, { sensitivity: 'base' });

const comparators = {
    newest: (a, b) => proposedAt(b).localeCompare(proposedAt(a)),
    oldest: (a, b) => proposedAt(a).localeCompare(proposedAt(b)),
    'most-comments': (a, b) =>
        b.attributes.prop_comments_number - a.attributes.prop_comments_number,
    'least-comments': (a, b) =>
        a.attributes.prop_comments_number - b.attributes.prop_comments_number,
    'name-asc': (a, b) => compareText(proposalName(a), proposalName(b)),
    'name-desc': (a, b) => compareText(proposalName(b), proposalName(a)),
    'proposer-asc': (a, b) => compareText(proposer(a), proposer(b)),
    'proposer-desc': (a, b) => compareText(proposer(b), proposer(a)),
};

// Search matches the proposal name or the proposer, ignoring case; an empty
// category list means every category.
export const filterArchiveItems = (
    items,
    { searchText = '', categoryIds = [], sortId = 'newest' } = {}
) => {
    const needle = searchText.trim().toLowerCase();
    const compare = comparators[sortId] ?? comparators.newest;
    return (items ?? [])
        .filter(
            (item) =>
                categoryIds.length === 0 ||
                categoryIds.includes(categoryId(item))
        )
        .filter(
            (item) =>
                !needle ||
                proposalName(item).toLowerCase().includes(needle) ||
                proposer(item).toLowerCase().includes(needle)
        )
        .sort(compare);
};

export const activeVersion = (versions) =>
    versions?.find((version) => version?.attributes?.is_active) ??
    versions?.[0] ??
    null;

// The poll the forum showed: the newest active one, else the newest.
export const displayedPoll = (polls) => {
    const newestFirst = [...(polls ?? [])].sort((a, b) =>
        b.attributes.createdAt.localeCompare(a.attributes.createdAt)
    );
    return (
        newestFirst.find((poll) => poll.attributes.is_poll_active) ??
        newestFirst[0] ??
        null
    );
};

// Top-level comments with their replies, in the order asked for; replies
// stay oldest first under their parent.
export const commentThreads = (comments, order = 'desc') => {
    const byCreated = (a, b) =>
        a.attributes.createdAt.localeCompare(b.attributes.createdAt);
    const replies = new Map();
    const roots = [];
    for (const comment of comments ?? []) {
        const parent = comment?.attributes?.comment_parent_id;
        if (parent === null || parent === undefined) {
            roots.push(comment);
        } else {
            const key = `${parent}`;
            replies.set(key, [...(replies.get(key) ?? []), comment]);
        }
    }
    roots.sort(order === 'asc' ? byCreated : (a, b) => byCreated(b, a));
    return roots.map((comment) => ({
        comment,
        replies: (replies.get(`${comment.id}`) ?? []).sort(byCreated),
    }));
};
