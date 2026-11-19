'use client';

import { Box, ButtonBase } from '@mui/material';
import { useEffect, useMemo, useRef, useState } from 'react';
import { useLocation } from 'react-router';
import { ListFilter } from 'lucide-react';
import { GovernanceActionTypeIcon } from '@/components/atoms/GovernanceActionTypeIcon';
import { FilterSidebar } from '@/components/molecules/FilterSidebar';
import { SearchField } from '@/components/molecules/SearchField';
import { SortSelect } from '@/components/molecules/SortSelect';
import { ds, RADIUS } from '@/consts/designTokens';
import { useScreenDimension } from '@/hooks/useScreenDimension';
import { CreateGovernanceActionDialog } from '../../components';
import ProposalRows from '../../components/ProposalRows';
import { getGovernanceActionTypes, getProposals } from '../../lib/api';
import { useAppContext } from '../../context/context';
import { useDebounce } from '../../lib/hooks';
import { countProposalsByType } from '../../lib/proposalList';
import { useReportProposeState } from '../../lib/proposeState';

// "Date Range: Latest": the option keys are the labels, so each option's id
// is `${label}-radio`, which the forum sort spec clicks.
const dateOptions = [
    { key: 'Latest', label: 'Latest', sort: { fieldId: 'createdAt', type: 'DESC' } },
    { key: 'Oldest', label: 'Oldest', sort: { fieldId: 'createdAt', type: 'ASC' } },
];

// Submitted proposals are on chain, and Live Voting lists them; Proposed
// keeps to the ones still under discussion.
const STATUSES = ['active'];

// The forum's type names (its seeded lookup) against GovTool's type icons.
const TYPE_ICONS = {
    'Info Action': 'InfoAction',
    'Treasury requests': 'TreasuryWithdrawals',
    'Updates to the Constitution': 'NewConstitution',
    'Motion of No Confidence': 'NoConfidence',
    'Hard fork': 'HardForkInitiation',
};

const ProposedGovernanceActions = () => {
    const location = useLocation();
    const { user, setOpenUsernameModal } = useAppContext();
    const { isMobile } = useScreenDimension();
    const [searchText, setSearchText] = useState('');
    const search = useDebounce(searchText.trim(), 500);
    const [dateKey, setDateKey] = useState(dateOptions[0].key);
    const [types, setTypes] = useState([]);
    const [selectedTypes, setSelectedTypes] = useState([]);
    const [counts, setCounts] = useState(undefined);
    const [areFiltersOpen, setAreFiltersOpen] = useState(false);
    const [showCreateGADialog, setShowCreateGADialog] = useState(false);
    const countsRequest = useRef(0);

    // GovTool's header shows the propose button and the sign-in notice.
    useReportProposeState(() => setShowCreateGADialog(true));

    useEffect(() => {
        getGovernanceActionTypes()
            .then((response) => {
                if (response?.data) setTypes(response.data);
            })
            .catch((error) => console.error(error));
    }, []);

    useEffect(() => {
        if (location.pathname.includes('propose')) {
            if (user?.user?.govtool_username) {
                setShowCreateGADialog(true);
            } else if (user) {
                setOpenUsernameModal({ open: true, callBackFn: () => {} });
            }
        }
    }, [location.pathname]);

    const typeIds = useMemo(() => types.map((type) => type?.id), [types]);

    // The sidebar counts follow the search; a later search wins.
    useEffect(() => {
        if (!typeIds.length) return;
        const current = countsRequest.current + 1;
        countsRequest.current = current;
        countProposalsByType(getProposals, {
            search,
            typeIds,
            statuses: STATUSES,
        })
            .then((next) => {
                if (countsRequest.current === current) setCounts(next);
            })
            .catch(() => {
                if (countsRequest.current === current) setCounts(undefined);
            });
    }, [search, typeIds]);

    const sort = (
        dateOptions.find((option) => option.key === dateKey) ?? dateOptions[0]
    ).sort;

    const sidebar = (
        <FilterSidebar
            allCount={counts?.all}
            allIcon={
                <ListFilter aria-hidden color={ds.primary['600']} size={16} />
            }
            allLabel='All Governance Actions'
            dataTestId='proposal-type-filter'
            onChange={setSelectedTypes}
            options={types.map((type) => {
                const name = type?.attributes?.gov_action_type_name ?? '';
                return {
                    key: String(type?.id),
                    label: name,
                    icon: TYPE_ICONS[name] ? (
                        <GovernanceActionTypeIcon type={TYPE_ICONS[name]} />
                    ) : undefined,
                    count: counts?.byType?.[type?.id],
                };
            })}
            selected={selectedTypes}
            title='Action type'
        />
    );

    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2.5 }}>
            <Box
                sx={{
                    alignItems: 'center',
                    display: 'flex',
                    flexWrap: { xxs: 'wrap', md: 'nowrap' },
                    gap: 1.5,
                }}
            >
                <SearchField
                    onChange={setSearchText}
                    onClear={() => setSearchText('')}
                    placeholder='Search proposals by name…'
                    value={searchText}
                />
                <ButtonBase
                    aria-expanded={areFiltersOpen}
                    data-testid='filters-button'
                    onClick={() => setAreFiltersOpen((open) => !open)}
                    sx={{
                        alignItems: 'center',
                        backgroundColor: '#FFFFFF',
                        border: `1px solid ${
                            selectedTypes.length
                                ? ds.primary['600']
                                : ds.gray['200']
                        }`,
                        borderRadius: `${RADIUS.md}px`,
                        color: ds.gray['700'],
                        display: { xxs: 'inline-flex', md: 'none' },
                        flex: { xxs: 1, md: 'none' },
                        fontFamily: 'inherit',
                        fontSize: 14,
                        gap: 0.75,
                        height: 40,
                        px: 1.5,
                        '&.Mui-focusVisible': {
                            outline: `2px solid ${ds.primary['600']}`,
                        },
                    }}
                >
                    <ListFilter aria-hidden size={16} />
                    Filters
                    {selectedTypes.length > 0 && ` (${selectedTypes.length})`}
                </ButtonBase>
                <SortSelect
                    label='Date Range:'
                    onChange={setDateKey}
                    options={dateOptions.map(({ key, label }) => ({ key, label }))}
                    value={dateKey}
                />
            </Box>

            {/* One sidebar in the DOM at a time, so its row ids stay unique. */}
            {isMobile && areFiltersOpen && sidebar}

            <Box
                sx={{
                    alignItems: 'start',
                    display: 'grid',
                    gap: 2.5,
                    gridTemplateColumns: {
                        xxs: '1fr',
                        md: '230px minmax(0, 1fr)',
                    },
                }}
            >
                {!isMobile && (
                    <Box
                        sx={{
                            maxHeight: 'calc(100vh - 104px)',
                            overflowY: 'auto',
                            position: 'sticky',
                            top: 88,
                        }}
                    >
                        {sidebar}
                    </Box>
                )}
                <Box sx={{ minWidth: 0 }}>
                    <ProposalRows
                        search={search}
                        sort={sort}
                        statuses={STATUSES}
                        typeIds={selectedTypes.map(Number)}
                    />
                </Box>
            </Box>

            {showCreateGADialog && (
                <CreateGovernanceActionDialog
                    open={showCreateGADialog}
                    onClose={() => setShowCreateGADialog(false)}
                />
            )}
        </Box>
    );
};

export default ProposedGovernanceActions;
