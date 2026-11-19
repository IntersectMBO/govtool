'use client';

import { useEffect, useRef, useState } from 'react';
import { Box, CircularProgress } from '@mui/material';

import { PaginationFooter } from '@/components/molecules/PaginationFooter';
import { ds, RADIUS } from '@/consts/designTokens';
import { getProposals } from '../../lib/api';
import {
    buildProposalListQuery,
    PROPOSALS_PAGE_SIZE,
} from '../../lib/proposalList';
import ProposalRow from '../ProposalRow';

// The proposals matching the page's filters, one page at a time.
const ProposalRows = ({ search, typeIds, statuses, sort }) => {
    const filterKey = JSON.stringify({ search, typeIds, statuses, sort });
    const [paging, setPaging] = useState({ key: filterKey, page: 1 });
    const [rows, setRows] = useState([]);
    const [total, setTotal] = useState(0);
    const [state, setState] = useState('loading');
    const [refreshKey, setRefreshKey] = useState(0);
    const request = useRef(0);

    // A new filter starts again from the first page, and forgets the old
    // page, so undoing the filter does not bring that page back.
    if (paging.key !== filterKey) setPaging({ key: filterKey, page: 1 });
    const page = paging.key === filterKey ? paging.page : 1;
    const setPage = (next) => setPaging({ key: filterKey, page: next });

    useEffect(() => {
        const current = request.current + 1;
        request.current = current;
        setState('loading');
        getProposals(
            buildProposalListQuery({ search, typeIds, statuses, sort, page })
        )
            .then((result) => {
                // A slower answer to an older filter never replaces a newer one.
                if (request.current !== current) return;
                if (!Array.isArray(result?.proposals)) {
                    setRows([]);
                    setTotal(0);
                    setState('error');
                    return;
                }
                const count = result.total ?? result.proposals.length;
                // Proposals can go away while a later page is open: move to
                // the last page that still has some instead of an empty one.
                if (!result.proposals.length && page > 1 && count > 0) {
                    setPage(Math.ceil(count / PROPOSALS_PAGE_SIZE));
                    return;
                }
                setRows(result.proposals);
                setTotal(count);
                setState('ready');
            })
            .catch(() => {
                if (request.current === current) setState('error');
            });
    }, [filterKey, page, refreshKey]);

    const refresh = (value) => {
        if (value) setRefreshKey((key) => key + 1);
    };

    if (state === 'loading' && rows.length === 0) {
        return (
            <Box sx={{ display: 'flex', justifyContent: 'center', py: 6 }}>
                <CircularProgress aria-label='Loading proposals' size={28} />
            </Box>
        );
    }

    if (state !== 'loading' && rows.length === 0) {
        return (
            <Box
                data-testid='proposals-empty-state'
                sx={{
                    backgroundColor: '#FFFFFF',
                    border: `1px solid ${ds.gray['200']}`,
                    borderRadius: `${RADIUS.lg}px`,
                    px: 3,
                    py: 5,
                    textAlign: 'center',
                }}
            >
                <Box
                    component='p'
                    sx={{
                        color: ds.gray['900'],
                        fontSize: 16,
                        fontWeight: 600,
                        m: 0,
                    }}
                >
                    {state === 'error'
                        ? 'Proposals could not be loaded'
                        : 'No proposals found'}
                </Box>
                <Box
                    component='p'
                    sx={{ color: ds.gray['600'], fontSize: 14, m: 0, mt: 0.5 }}
                >
                    {state === 'error'
                        ? 'Check your connection and try again in a moment.'
                        : 'Try a different search or filter.'}
                </Box>
            </Box>
        );
    }

    return (
        <Box
            aria-busy={state === 'loading'}
            sx={{
                display: 'flex',
                flexDirection: 'column',
                gap: 1.5,
                opacity: state === 'loading' ? 0.6 : 1,
                transition: 'opacity 0.15s',
            }}
        >
            {rows.map((proposal) => (
                <ProposalRow
                    key={proposal?.id}
                    proposal={proposal}
                    setShouldRefresh={refresh}
                />
            ))}
            {total > PROPOSALS_PAGE_SIZE && (
                <PaginationFooter
                    onPageChange={(next) => {
                        setPage(next);
                        window.scrollTo({ top: 0, behavior: 'smooth' });
                    }}
                    onPageSizeChange={() => {}}
                    page={page}
                    pageSize={PROPOSALS_PAGE_SIZE}
                    total={total}
                />
            )}
        </Box>
    );
};

export default ProposalRows;
