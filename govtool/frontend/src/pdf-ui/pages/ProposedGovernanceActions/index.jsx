'use client';

import { useTheme } from '@emotion/react';
import {
    IconPlusCircle,
    IconArrowDown,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import ArrowDownwardIcon from '@mui/icons-material/ArrowDownward';
import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';
import SearchIcon from '@mui/icons-material/Search';
import { ICONS } from '@/consts/icons';
import {
    Box,
    Checkbox,
    FormControlLabel,
    IconButton,
    InputAdornment,
    Menu,
    MenuItem,
    TextField,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useState } from 'react';
import {
    ProposalsList,
    CreateGovernanceActionDialog,
    SearchInput,
} from '../../components';
import { getGovernanceActionTypes } from '../../lib/api';
import { useAppContext } from '../../context/context';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    loginUserToApp,
} from '../../lib/helpers';
import { useLocation } from 'react-router';
import { useScreenDimension } from '@/hooks/useScreenDimension';
import { decodeJWT } from '../../lib/utils';
import UserValidation from '../../components/UserValidation/UserValidation';
import { primaryBlue } from '@/consts/colors';

let sortOptions = [
    { fieldId: 'createdAt', type: 'DESC', title: 'Newest' },
    { fieldId: 'createdAt', type: 'ASC', title: 'Oldest' },
    {
        fieldId: 'proposal][prop_likes',
        type: 'DESC',
        title: 'Most likes',
    },
    {
        fieldId: 'proposal][prop_likes',
        type: 'ASC',
        title: 'Least likes',
    },
    {
        fieldId: 'proposal][prop_dislikes',
        type: 'DESC',
        title: 'Most dislikes',
    },
    {
        fieldId: 'proposal][prop_dislikes',
        type: 'ASC',
        title: 'Least dislikes',
    },
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
    {
        fieldId: 'prop_name',
        type: 'ASC',
        title: 'Name A-Z',
    },
    {
        fieldId: 'prop_name',
        type: 'DESC',
        title: 'Name Z-A',
    },
];

// GovTool's DataActionsBar look: OrderActionsChip pills for the filter and
// sort triggers, and the DataActionsFilters/DataActionsSorting dropdowns.
const chipButtonSx = (open) => ({
    textTransform: 'none',
    borderRadius: '99px',
    padding: '12px 14px',
    height: 'auto',
    fontSize: 16,
    fontWeight: 500,
    border: 'none',
    boxShadow: 'none',
    bgcolor: open ? 'secondary.main' : 'transparent',
    color: open ? 'white' : 'primaryBlue',
    '& .MuiButton-startIcon': { mr: 1, ml: 0 },
    '&:hover': {
        bgcolor: open ? 'secondary.main' : primaryBlue.c50,
        boxShadow: 'none',
    },
});

const menuPaperSx = {
    overflow: 'visible',
    mt: 1,
    background: '#FBFBFF',
    boxShadow: '1px 2px 11px 0px #00123D5E',
    borderRadius: '10px',
    padding: '12px 0px',
    width: { xxs: '250px', md: '415px' },
};

const menuTitleSx = {
    fontSize: 14,
    fontWeight: 500,
    color: '#9792B5',
    px: '20px',
    mb: 0.5,
};

const menuItemSx = {
    px: '20px',
    '&:hover': { bgcolor: '#E6EBF7' },
    '&.Mui-selected, &.Mui-selected:hover, &.Mui-selected.Mui-focusVisible': {
        bgcolor: '#FFF0E7',
    },
};

const ProposedGovernanceActions = () => {
    const location = useLocation();
    const theme = useTheme();
    const { isMobile } = useScreenDimension();
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
    const [proposalSearchText, setProposalSearchText] = useState('');
    const [sortType, setSortType] = useState(sortOptions[0]);
    const [governanceActionTypeList, setGovernanceActionTypeList] = useState(
        []
    );
    const [
        filteredGovernanceActionTypeList,
        setFilteredGovernanceActionTypeList,
    ] = useState([]);
    const [showCreateGADialog, setShowCreateGADialog] = useState(false);
    const [
        filteredGovernanceActionStatusList,
        setFilteredGovernanceActionStatusList,
    ] = useState(['active']);

    const [filtersAnchorEl, setFiltersAnchorEl] = useState(null);
    const [showAllActivated, setShowAllActivated] = useState({
        is_activated: false,
        gov_action_type: null,
    });

    const openFilters = Boolean(filtersAnchorEl);
    const handleFiltersClick = (event) => {
        setFiltersAnchorEl(event.currentTarget);
    };
    const handleCloseFilters = () => {
        setFiltersAnchorEl(null);
    };

    const [sortAnchorEl, setSortAnchorEl] = useState(null);
    const openSort = Boolean(sortAnchorEl);

    const handleSortClick = (event) => {
        setSortAnchorEl(event.currentTarget);
    };
    const handleSortClose = () => {
        setSortAnchorEl(null);
    };

    const fetchGovernanceActionTypes = async () => {
        try {
            let response = await getGovernanceActionTypes();

            if (!response?.data) return;

            setGovernanceActionTypeList(response?.data);
        } catch (error) {
            console.error(error);
        }
    };

    const toggleActionFilter = (action) => {
        let filterExist = filteredGovernanceActionTypeList?.some(
            (filter) => filter?.id === action?.id
        );

        let updatedList;
        if (filterExist) {
            updatedList = filteredGovernanceActionTypeList.filter(
                (filter) => filter?.id !== action?.id
            );
        } else {
            updatedList = [...filteredGovernanceActionTypeList, action];
        }

        updatedList.sort((a, b) => a?.id - b?.id);

        setFilteredGovernanceActionTypeList(updatedList);
    };

    const toggleStatusFilter = (status) => {
        let filterExist = filteredGovernanceActionStatusList?.some(
            (filter) => filter === status
        );

        let updatedList;
        if (filterExist) {
            updatedList = filteredGovernanceActionStatusList.filter(
                (filter) => filter !== status
            );
        } else {
            updatedList = [...filteredGovernanceActionStatusList, status];
        }

        setFilteredGovernanceActionStatusList(updatedList);
    };

    const resetFilters = () => {
        setFilteredGovernanceActionTypeList([]);
        setFilteredGovernanceActionStatusList(['active']);
        handleCloseFilters();
    };

    useEffect(() => {
        fetchGovernanceActionTypes();
    }, []);

    useEffect(() => {
        if (showAllActivated?.is_activated) {
            setFilteredGovernanceActionTypeList([
                showAllActivated?.gov_action_type,
            ]);
        } else {
            setFilteredGovernanceActionTypeList([]);
        }
    }, [showAllActivated]);

    useEffect(() => {
        if (location.pathname.includes('propose')) {
            if (user?.user?.govtool_username) {
                setShowCreateGADialog(true);
            } else if (user) {
                setOpenUsernameModal({ open: true, callBackFn: () => {} });
            }
        }
    }, [location.pathname]);

    return (
        <Box sx={{ mt: 3 }}>
            <Box display={'flex'} flexDirection={'column'}>
                {!walletAPI?.address && (
                    <Typography
                        variant={isMobile ? 'title1' : 'headline3'}
                        component='h1'
                        sx={{ mb: isMobile ? 3.75 : 6 }}
                    >
                        Proposed Governance Actions
                    </Typography>
                )}

                {showAllActivated?.is_activated && (
                    <Box mb={2}>
                        <Button
                            variant='text'
                            size='medium'
                            startIcon={
                                <ArrowBackIosIcon color='primary' />
                            }
                            onClick={() => {
                                setShowAllActivated({
                                    is_activated: false,
                                    gov_action_type: null,
                                });
                            }}
                            data-testid='back-to-proposal-discussion-button'
                        >
                            Back to Proposal Discussion
                        </Button>
                    </Box>
                )}
                <Box
                    sx={{
                        display: 'flex',
                        flexDirection: 'row',
                        flexWrap: 'wrap',
                        alignItems: 'center',
                        gap: 1,
                        mb: 3,
                    }}
                >
                    <Button
                        variant='contained'
                        disabled={checkShowValidation(
                            false,
                            walletAPI,
                            user
                        )}
                        onClick={async () =>
                            await loginUserToApp({
                                wallet: walletAPI,
                                setUser: setUser,
                                setOpenUsernameModal:
                                    setOpenUsernameModal,
                                callBackFn: () =>
                                    setShowCreateGADialog(true),
                                clearStates: clearStates,
                                addErrorAlert: addErrorAlert,
                                addSuccessAlert: addSuccessAlert,
                                addChangesSavedAlert:
                                    addChangesSavedAlert,
                            })
                        }
                        // startIcon={<IconPlusCircle fill='white' />}
                        data-testid='propose-a-governance-action-button'
                    >
                        Propose a Governance Action
                    </Button>
                    {checkShowValidation(
                        false,
                        walletAPI,
                        user
                    ) && (
                        <UserValidation
                            type='proposal'
                            drepCheck={checkIfDrepIsSignedIn(
                                walletAPI
                            )}
                            drepRequired={false}
                        />
                    )}
                </Box>

                <Box
                    sx={{
                        display: 'flex',
                        flexWrap: 'wrap',
                        alignItems: 'center',
                        columnGap: { xxs: 1, sm: 1.5 },
                        rowGap: 1,
                    }}
                >
                    <Box sx={{ flex: '1 1 280px', minWidth: 0 }}>
                        <SearchInput
                            onDebouncedChange={
                                setProposalSearchText
                            }
                            placeholder='Search...'
                        />
                    </Box>
                    <Box
                        sx={{
                            display: 'flex',
                            alignItems: 'center',
                            gap: { xxs: 0.5, md: 1.5 },
                            flex: '0 0 auto',
                        }}
                    >
                        <Button
                            variant='text'
                            onClick={handleFiltersClick}
                            startIcon={
                                <img
                                    src={
                                        openFilters
                                            ? ICONS.filterWhiteIcon
                                            : ICONS.filterIcon
                                    }
                                    alt=''
                                    width={20}
                                    height={20}
                                />
                            }
                            id='filters-button'
                            data-testid='filter-button'
                            sx={chipButtonSx(openFilters)}
                            aria-controls={
                                openFilters ? 'filters-menu' : undefined
                            }
                            aria-haspopup='true'
                            aria-expanded={
                                openFilters ? 'true' : undefined
                            }
                        >
                            {' '}
                            Filter:
                        </Button>
                        <Menu
                            id='filters-menu'
                            anchorEl={filtersAnchorEl}
                            open={openFilters}
                            onClose={handleCloseFilters}
                            MenuListProps={{
                                'aria-labelledby': 'filters-button',
                            }}
                            slotProps={{
                                paper: {
                                    elevation: 0,
                                    sx: menuPaperSx,
                                },
                            }}
                            transformOrigin={{
                                horizontal: 'right',
                                vertical: 'top',
                            }}
                            anchorOrigin={{
                                horizontal: 'right',
                                vertical: 'bottom',
                            }}
                        >
                            <Box>
                                {!showAllActivated?.is_activated && (
                                    <Box>
                                        <Typography sx={menuTitleSx}>
                                            Proposal types
                                        </Typography>
                                        {governanceActionTypeList?.map(
                                            (ga, index) => (
                                                <MenuItem
                                                    key={`${ga?.attributes?.gov_action_type_name}-${index}`}
                                                    selected={filteredGovernanceActionTypeList?.some(
                                                        (filter) =>
                                                            filter?.id ===
                                                            ga?.id
                                                    )}
                                                    sx={menuItemSx}
                                                    id={`${ga?.attributes?.gov_action_type_name}-radio-wrapper`}
                                                    data-testid={
                                                        ga?.attributes
                                                            ?.gov_action_type_name
                                                            ? `${ga?.attributes?.gov_action_type_name?.toLowerCase()}-radio-wrapper`
                                                            : `${index}-radio-wrapper`
                                                    }
                                                >
                                                    <FormControlLabel
                                                        sx={{ m: 0, width: '100%' }}
                                                        control={
                                                            <Checkbox
                                                                onChange={() =>
                                                                    toggleActionFilter(
                                                                        ga
                                                                    )
                                                                }
                                                                checked={filteredGovernanceActionTypeList?.some(
                                                                    (
                                                                        filter
                                                                    ) =>
                                                                        filter?.id ===
                                                                        ga?.id
                                                                )}
                                                                id={`${ga?.attributes?.gov_action_type_name}-radio`}
                                                                data-testid={
                                                                    ga
                                                                        ?.attributes
                                                                        ?.gov_action_type_name
                                                                        ? `${ga?.attributes?.gov_action_type_name?.toLowerCase()}-radio`
                                                                        : `${index}-radio`
                                                                }
                                                            />
                                                        }
                                                        label={
                                                            ga
                                                                ?.attributes
                                                                ?.gov_action_type_name
                                                        }
                                                    />
                                                </MenuItem>
                                            )
                                        )}
                                    </Box>
                                )}

                                <Typography
                                    sx={{ ...menuTitleSx, mt: 2 }}
                                >
                                    Proposal status
                                </Typography>
                                <MenuItem
                                    selected={filteredGovernanceActionStatusList?.some(
                                        (filter) =>
                                            filter === 'submitted'
                                    )}
                                    sx={menuItemSx}
                                    id={`submitted-for-vote-radio-wrapper`}
                                    data-testid={`submitted-for-vote-radio-wrapper`}
                                >
                                    <FormControlLabel
                                        sx={{ m: 0, width: '100%' }}
                                        control={
                                            <Checkbox
                                                onChange={() =>
                                                    toggleStatusFilter(
                                                        'submitted'
                                                    )
                                                }
                                                checked={filteredGovernanceActionStatusList?.some(
                                                    (filter) =>
                                                        filter ===
                                                        'submitted'
                                                )}
                                                id={`submitted-for-vote-radio`}
                                                data-testid={`submitted-for-vote-radio`}
                                            />
                                        }
                                        label={'Submitted for vote'}
                                    />
                                </MenuItem>
                                <MenuItem
                                    selected={filteredGovernanceActionStatusList?.some(
                                        (filter) => filter === 'active'
                                    )}
                                    sx={menuItemSx}
                                    id={`active-proposal-radio-wrapper`}
                                    data-testid={`active-proposal-radio-wrapper`}
                                >
                                    <FormControlLabel
                                        sx={{ m: 0, width: '100%' }}
                                        control={
                                            <Checkbox
                                                onChange={() =>
                                                    toggleStatusFilter(
                                                        'active'
                                                    )
                                                }
                                                checked={filteredGovernanceActionStatusList?.some(
                                                    (filter) =>
                                                        filter ===
                                                        'active'
                                                )}
                                                id={`active-proposal-radio`}
                                                data-testid={`active-proposal-radio`}
                                            />
                                        }
                                        label={'Active proposal'}
                                    />
                                </MenuItem>

                                <MenuItem
                                    onClick={() => resetFilters()}
                                    data-testid='reset-filters'
                                    sx={{ ...menuItemSx, mt: 1 }}
                                >
                                    {' '}
                                    <Typography
                                        fontSize={14}
                                        fontWeight={500}
                                        color={'primary'}
                                    >
                                        Reset filters
                                    </Typography>
                                </MenuItem>
                            </Box>
                        </Menu>
                        <>
                            <Button
                                variant='text'
                                onClick={(e) => handleSortClick(e)}
                                startIcon={
                                    <img
                                        src={
                                            openSort
                                                ? ICONS.sortWhiteIcon
                                                : ICONS.sortIcon
                                        }
                                        alt=''
                                        width={20}
                                        height={20}
                                    />
                                }
                                sx={{
                                    ...chipButtonSx(openSort),
                                    whiteSpace: 'nowrap',
                                }}
                                data-testid='sort-button'
                            >
                                Sort: {sortType.title}
                            </Button>
                            <Menu
                                id='sort-menu'
                                anchorEl={sortAnchorEl}
                                open={openSort}
                                onClose={handleSortClose}
                                MenuListProps={{
                                    'aria-labelledby': 'sort-button',
                                }}
                                slotProps={{
                                    paper: {
                                        elevation: 0,
                                        sx: menuPaperSx,
                                    },
                                }}
                                transformOrigin={{
                                    horizontal: 'right',
                                    vertical: 'top',
                                }}
                                anchorOrigin={{
                                    horizontal: 'right',
                                    vertical: 'bottom',
                                }}
                            >
                                <Box>
                                    {sortOptions.map((sort, index) => (
                                        <MenuItem
                                            key={`${sort?.title}-${index}`}
                                            selected={sort === sortType}
                                            id={`${sort?.title}`}
                                            data-testid={`${sort?.title}-sort-option`}
                                            onClick={() => {
                                                setSortType(sort);
                                                handleSortClose();
                                            }}
                                            sx={{
                                                ...menuItemSx,
                                                width: '100%',
                                                fontSize: 16,
                                            }}
                                        >
                                            {sort.title}
                                        </MenuItem>
                                    ))}
                                </Box>
                            </Menu>
                        </>
                        {/* <Button
                            variant='outlined'
                            onClick={() =>
                                setSortType((prev) =>
                                    prev === 'desc' ? 'asc' : 'desc'
                                )
                            }
                            endIcon={
                                sortType === 'desc' ? (
                                    <ArrowDownwardIcon sx={{ color: 'textBlack' }} />
                                ) : (
                                    <ArrowUpwardIcon sx={{ color: 'textBlack' }} />
                                    // <IconArrowDown />
                                )
                            }
                            sx={{
                                textTransform: 'none',
                                borderRadius: '20px',
                                padding: '8px 16px',
                                borderColor: 'primary.main',
                                color: 'textBlack',
                                '&:hover': {
                                    backgroundColor: primaryBlue.c50,
                                },
                            }}
                            data-testid='sort-button'
                        >
                            Sort:{' '}
                            {sortType === 'desc'
                                ? 'Last modified (desc)'
                                : 'Last modified (asc)'}
                        </Button> */}
                    </Box>
                </Box>
            </Box>

            <Box
                sx={{
                    display: 'flex',
                    flexDirection: 'column',
                    gap: 5,
                    mt: isMobile ? 7.5 : 10,
                }}
            >
                {(filteredGovernanceActionTypeList?.length > 0
                    ? filteredGovernanceActionTypeList
                    : governanceActionTypeList
                )?.map((item, index) => (
                    <Box
                        key={`${item?.attributes?.gov_action_type_name}-${index}`}
                    >
                        <ProposalsList
                            governanceAction={item}
                            searchText={proposalSearchText}
                            sortType={sortType}
                            statusList={filteredGovernanceActionStatusList}
                            setShowAllActivated={setShowAllActivated}
                            showAllActivated={showAllActivated}
                        />
                    </Box>
                ))}
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
