'use client';

import { Box } from '@mui/material';
import { Button } from '@atoms';
import { useEffect, useMemo, useState } from 'react';
import { useLocation } from 'react-router';
import { SearchField } from '@/components/molecules/SearchField';
import { SortSelect } from '@/components/molecules/SortSelect';
import { CreateGovernanceActionDialog } from '../../components';
import ProposalFilterMenu from '../../components/ProposalFilterMenu';
import ProposalRows from '../../components/ProposalRows';
import { getGovernanceActionTypes } from '../../lib/api';
import { useAppContext } from '../../context/context';
import { useDebounce } from '../../lib/hooks';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    loginUserToApp,
} from '../../lib/helpers';
import UserValidation from '../../components/UserValidation/UserValidation';

// The titles double as the option keys, so each option's id is
// `${title}-radio`, which the forum sort spec clicks.
const sortOptions = [
    { fieldId: 'createdAt', type: 'DESC', title: 'Newest' },
    { fieldId: 'createdAt', type: 'ASC', title: 'Oldest' },
    { fieldId: 'proposal][prop_likes', type: 'DESC', title: 'Most likes' },
    { fieldId: 'proposal][prop_likes', type: 'ASC', title: 'Least likes' },
    { fieldId: 'proposal][prop_dislikes', type: 'DESC', title: 'Most dislikes' },
    { fieldId: 'proposal][prop_dislikes', type: 'ASC', title: 'Least dislikes' },
    {
        fieldId: 'proposal][prop_comments_number',
        type: 'DESC',
        title: 'Most comments',
    },
    {
        fieldId: 'proposal][prop_comments_number',
        type: 'ASC',
        title: 'Least comments',
    },
    { fieldId: 'prop_name', type: 'ASC', title: 'Name A-Z' },
    { fieldId: 'prop_name', type: 'DESC', title: 'Name Z-A' },
];

const statusOptions = [
    { key: 'active', label: 'Active', testId: 'active-proposal' },
    { key: 'submitted', label: 'Submitted for vote', testId: 'submitted-for-vote' },
];

const toggle = (list, key) =>
    list.includes(key) ? list.filter((item) => item !== key) : [...list, key];

const ProposedGovernanceActions = () => {
    const location = useLocation();
    const {
        user,
        walletAPI,
        setOpenUsernameModal,
        setUser,
        clearStates,
        addSuccessAlert,
        addErrorAlert,
        addChangesSavedAlert,
    } = useAppContext();
    const [searchText, setSearchText] = useState('');
    const search = useDebounce(searchText.trim(), 500);
    const [sortTitle, setSortTitle] = useState(sortOptions[0].title);
    const [governanceActionTypeList, setGovernanceActionTypeList] = useState(
        []
    );
    const [selectedTypes, setSelectedTypes] = useState([]);
    // Submitted proposals have moved on chain, so the list opens on the
    // active ones, as it always has.
    const [selectedStatuses, setSelectedStatuses] = useState(['active']);
    const [showCreateGADialog, setShowCreateGADialog] = useState(false);

    useEffect(() => {
        getGovernanceActionTypes()
            .then((response) => {
                if (response?.data) setGovernanceActionTypeList(response.data);
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

    const typeOptions = useMemo(
        () =>
            governanceActionTypeList.map((type, index) => {
                const name = type?.attributes?.gov_action_type_name ?? '';
                return {
                    key: String(type?.id),
                    label: name,
                    testId: name ? name.toLowerCase() : String(index),
                };
            }),
        [governanceActionTypeList]
    );
    const sort =
        sortOptions.find((option) => option.title === sortTitle) ??
        sortOptions[0];
    const typeIds = selectedTypes.map(Number);

    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 2 }}>
            {/* GovTool's page header above the forum carries the title
                and the Proposed / Live Voting / Outcomes tabs. The forum's
                own propose button stays: its sign-in prompt sits beside it. */}
            <Box
                sx={{
                    alignItems: 'center',
                    display: 'flex',
                    flexWrap: 'wrap',
                    gap: 1,
                    justifyContent: 'flex-end',
                }}
            >
                {checkShowValidation(false, walletAPI, user) && (
                    <UserValidation
                        type='proposal'
                        drepCheck={checkIfDrepIsSignedIn(walletAPI)}
                        drepRequired={false}
                    />
                )}
                <Button
                    variant='outlined'
                    disabled={checkShowValidation(false, walletAPI, user)}
                    onClick={async () =>
                        await loginUserToApp({
                            wallet: walletAPI,
                            setUser: setUser,
                            setOpenUsernameModal: setOpenUsernameModal,
                            callBackFn: () => setShowCreateGADialog(true),
                            clearStates: clearStates,
                            addErrorAlert: addErrorAlert,
                            addSuccessAlert: addSuccessAlert,
                            addChangesSavedAlert: addChangesSavedAlert,
                        })
                    }
                    data-testid='propose-a-governance-action-button'
                >
                    Propose a Governance Action
                </Button>
            </Box>

            <Box
                sx={{
                    alignItems: 'center',
                    display: 'flex',
                    flexWrap: 'wrap',
                    gap: 1.5,
                }}
            >
                <SearchField
                    onChange={setSearchText}
                    onClear={() => setSearchText('')}
                    placeholder='Search proposals by name…'
                    value={searchText}
                />
                <SortSelect
                    onChange={setSortTitle}
                    options={sortOptions.map((option) => ({
                        key: option.title,
                        label: option.title,
                    }))}
                    value={sortTitle}
                />
                <ProposalFilterMenu
                    dataTestId='filter-button'
                    label='Proposal type:'
                    onClear={() => setSelectedTypes([])}
                    onToggle={(key) =>
                        setSelectedTypes((list) => toggle(list, key))
                    }
                    options={typeOptions}
                    selected={selectedTypes}
                />
                <ProposalFilterMenu
                    dataTestId='status-filter-button'
                    label='Status:'
                    onToggle={(key) =>
                        setSelectedStatuses((list) => toggle(list, key))
                    }
                    options={statusOptions}
                    selected={selectedStatuses}
                />
            </Box>

            <ProposalRows
                search={search}
                sort={sort}
                statuses={selectedStatuses}
                typeIds={typeIds}
            />

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
