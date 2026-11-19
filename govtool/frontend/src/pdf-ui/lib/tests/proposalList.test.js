import { describe, expect, it } from 'vitest';

import {
    buildProposalListQuery,
    countProposalsByType,
    supportPercent,
} from '../proposalList';

const TAIL =
    '&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal';

describe('buildProposalListQuery', () => {
    it('asks for every type and status, newest first, by default', () => {
        expect(buildProposalListQuery({})).toBe(
            `filters[$and][0][prop_name][$containsi]=&pagination[page]=1&pagination[pageSize]=10&sort[createdAt]=DESC${TAIL}`
        );
    });

    it('filters on the chosen types in one request', () => {
        expect(
            buildProposalListQuery({ typeIds: [1, 4], page: 2 })
        ).toBe(
            `filters[$and][0][prop_name][$containsi]=&filters[$and][1][gov_action_type_id][$in][0]=1&filters[$and][1][gov_action_type_id][$in][1]=4&pagination[page]=2&pagination[pageSize]=10&sort[createdAt]=DESC${TAIL}`
        );
    });

    it('filters on one status and ignores both or none', () => {
        expect(buildProposalListQuery({ statuses: ['submitted'] })).toContain(
            '&filters[$and][2][prop_submitted]=true&'
        );
        expect(buildProposalListQuery({ statuses: ['active'] })).toContain(
            '&filters[$and][2][prop_submitted]=false&'
        );
        expect(
            buildProposalListQuery({ statuses: ['active', 'submitted'] })
        ).not.toContain('prop_submitted');
    });

    it('keeps the sort right before populate, as the sort specs wait for', () => {
        expect(
            buildProposalListQuery({
                sort: { fieldId: 'proposal][prop_likes', type: 'ASC' },
            })
        ).toContain(`&sort[proposal][prop_likes]=ASC&populate[0]=proposal_links`);
    });

    it('encodes the search so it cannot add query parameters', () => {
        expect(
            buildProposalListQuery({ search: 'a&filters[x]=1 b' })
        ).toContain('[$containsi]=a%26filters%5Bx%5D%3D1%20b&');
    });
});

describe('supportPercent', () => {
    it('is the share of likes among likes and dislikes', () => {
        expect(supportPercent(3, 1)).toBe(75);
        expect(supportPercent(2, 1)).toBe(67);
    });

    it('is null with no likes or dislikes', () => {
        expect(supportPercent(0, 0)).toBeNull();
        expect(supportPercent(undefined, null)).toBeNull();
    });
});

describe('countProposalsByType', () => {
    it('asks for one row per type and for all, and keeps each total', async () => {
        const queries = [];
        const fetch = async (query) => {
            queries.push(query);
            const type = query.match(/\[\$in\]\[0\]=(\d+)/)?.[1];
            return { proposals: [], total: type ? Number(type) * 10 : 99 };
        };
        await expect(
            countProposalsByType(fetch, { search: 'x', typeIds: [1, 2] })
        ).resolves.toEqual({ all: 99, byType: { 1: 10, 2: 20 } });
        expect(queries).toHaveLength(3);
        expect(queries.every((q) => q.includes('pagination[pageSize]=1'))).toBe(true);
        expect(queries.every((q) => q.includes('[$containsi]=x&'))).toBe(true);
    });

    it('counts only the chosen statuses', async () => {
        const queries = [];
        const fetch = async (query) => {
            queries.push(query);
            return { proposals: [], total: 1 };
        };
        await countProposalsByType(fetch, {
            search: '',
            typeIds: [1],
            statuses: ['active'],
        });
        expect(queries.every((q) => q.includes('[prop_submitted]=false'))).toBe(true);
    });

    it('gives up on the counts when any request fails', async () => {
        const fetch = async (query) =>
            query.includes('[$in]') ? new Error('Network Error') : { total: 3 };
        await expect(
            countProposalsByType(fetch, { search: '', typeIds: [1] })
        ).resolves.toBeUndefined();
    });
});
