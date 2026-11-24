// The proposals list query: one request for every chosen type, so the list
// can page across types. The forum backend's PROPOSALS_ALLOWLIST takes any
// scalar filter and sort on proposal rows; its pdf-ui corpus test pins this
// shape (govtool-pdf-backend/test/helpers/pdf-ui-corpus.ts).

const POPULATE =
    'populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal';

export const PROPOSALS_PAGE_SIZE = 10;

export const buildProposalListQuery = ({
    search = '',
    typeIds = [],
    statuses = [],
    sort = { fieldId: 'createdAt', type: 'DESC' },
    page = 1,
    pageSize = PROPOSALS_PAGE_SIZE,
}) => {
    const parts = [
        `filters[$and][0][prop_name][$containsi]=${encodeURIComponent(search)}`,
    ];
    typeIds.forEach((id, index) => {
        parts.push(
            `filters[$and][1][gov_action_type_id][$in][${index}]=${encodeURIComponent(id)}`
        );
    });
    // Both statuses, or none, is every proposal.
    if (statuses.length === 1) {
        parts.push(
            `filters[$and][2][prop_submitted]=${statuses[0] === 'submitted' ? 'true' : 'false'}`
        );
    }
    parts.push(
        `pagination[page]=${page}`,
        `pagination[pageSize]=${pageSize}`,
        `sort[${sort.fieldId}]=${sort.type}`,
        POPULATE
    );
    return parts.join('&');
};

/** Likes as a whole-number share of likes and dislikes; null without any. */
export const supportPercent = (likes, dislikes) => {
    const up = Number(likes) || 0;
    const down = Number(dislikes) || 0;
    if (up + down === 0) return null;
    return Math.round((up * 100) / (up + down));
};

const totalOf = (result) =>
    Array.isArray(result?.proposals) || typeof result?.total === 'number'
        ? result?.total
        : undefined;

/**
 * How many proposals match the search, in all and per type: one single-row
 * request each, read for its total. Undefined when any of them fails.
 */
export const countProposalsByType = async (
    fetchProposals,
    { search, typeIds, statuses = [] }
) => {
    const [all, ...perType] = await Promise.all([
        fetchProposals(buildProposalListQuery({ search, statuses, pageSize: 1 })),
        ...typeIds.map((id) =>
            fetchProposals(
                buildProposalListQuery({
                    search,
                    statuses,
                    typeIds: [id],
                    pageSize: 1,
                })
            )
        ),
    ]);
    const totals = [all, ...perType].map(totalOf);
    if (totals.some((total) => typeof total !== 'number')) return undefined;
    return {
        all: totals[0],
        byType: Object.fromEntries(typeIds.map((id, i) => [id, totals[i + 1]])),
    };
};
