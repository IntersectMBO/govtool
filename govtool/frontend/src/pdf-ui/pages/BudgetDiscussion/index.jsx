'use client';

import { useTheme } from '@emotion/react';
import {
    IconPlusCircle,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import ArrowDownwardIcon from '@mui/icons-material/ArrowDownward';
import ArrowUpwardIcon from '@mui/icons-material/ArrowUpward';
import { ICONS } from '@/consts/icons';
import {
    Box,
    Checkbox,
    FormControlLabel,
    InputAdornment,
    Menu,
    MenuItem,
    TextField,
    Card,
    CardContent,
    Stack,
    Radio,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useState } from 'react';
import { getBudgetDiscussionTypes } from '../../lib/api';
import {
    CreateBudgetDiscussionDialog,
    BudgetDiscussionsList,
    SearchInput,
} from '../../components';
import { useAppContext } from '../../context/context';
import {
    checkIfDrepIsSignedIn,
    checkShowValidation,
    loginUserToApp,
} from '../../lib/helpers';
import { useLocation } from 'react-router';
import { useScreenDimension } from '@/hooks/useScreenDimension';
import { ScrollToTop, useDebounce } from '../../lib/hooks';
import UserValidation from '../../components/UserValidation/UserValidation';
import { primaryBlue } from '@/consts/colors';

let proposalsOwnersList = [
    { id: 'all-proposals', label: 'All Proposals', disabled: false },
    { id: 'my-proposals', label: 'My Proposals', disabled: true },
];
let sortOptions = [
    { fieldId: 'createdAt', type: 'DESC', title: 'Newest' },
    { fieldId: 'createdAt', type: 'ASC', title: 'Oldest' },
    { fieldId: 'prop_comments_number', type: 'DESC', title: 'Most comments' },
    { fieldId: 'prop_comments_number', type: 'ASC', title: 'Least comments' },
    {
        fieldId: 'bd_proposal_detail][proposal_name',
        type: 'ASC',
        title: 'Name A-Z',
    },
    {
        fieldId: 'bd_proposal_detail][proposal_name',
        type: 'DESC',
        title: 'Name Z-A',
    },
    {
        fieldId: 'creator][govtool_username',
        type: 'ASC',
        title: 'Proposer A-Z',
    },
    {
        fieldId: 'creator][govtool_username',
        type: 'DESC',
        title: 'Proposer Z-A',
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

const ProposedBudgetDiscussion = () => {
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

    const defaultOwnerFilterId = 'all-proposals';
    const defaultOwnerFilter = proposalsOwnersList?.find(
        (f) => f?.id === defaultOwnerFilterId
    );

    const [budgetDiscussionSearchText, setBudgetDiscussionSearchText] =
        useState('');
    const [sortType, setSortType] = useState(sortOptions[0]);
    const [budgetDiscussionTypeList, setBudgetDiscussionTypeList] = useState(
        []
    );
    const [
        filteredBudgetDiscussionTypeList,
        setFilteredBudgetDiscussionTypeList,
    ] = useState([]);
    const [proposalsOwnerFilter, setProposalsOwnerFilter] =
        useState(defaultOwnerFilter);

    const [showCreateBDDialog, setShowCreateBDDialog] = useState(false);
    const [
        filteredBudgetDiscussionStatusList,
        setFilteredBudgetDiscussionStatusList,
    ] = useState(['active']);
    const [filtersAnchorEl, setFiltersAnchorEl] = useState(null);
    const [showAllActivated, setShowAllActivated] = useState({
        is_activated: false,
        bd_type: null,
    });
    const [sortAnchorEl, setSortAnchorEl] = useState(null);

    const [isAllProposalsListEmpty, setIsAllProposalsListEmpty] = useState([]);

    const openFilters = Boolean(filtersAnchorEl);
    const openSort = Boolean(sortAnchorEl);
    const handleFiltersClick = (event) => {
        setFiltersAnchorEl(event.currentTarget);
    };
    const handleCloseFilters = () => {
        setFiltersAnchorEl(null);
    };

    const handleSortClick = (event) => {
        setSortAnchorEl(event.currentTarget);
    };
    const handleSortClose = () => {
        setSortAnchorEl(null);
    };
    const fetchBudgetDiscussionTypes = async () => {
        try {
            let response = await getBudgetDiscussionTypes();
            if (!response?.data) return;
            setBudgetDiscussionTypeList(response?.data);
        } catch (error) {
            console.error(error);
        }
    };

    const toggleActionFilter = (action) => {
        let filterExist = filteredBudgetDiscussionTypeList?.some(
            (filter) => filter?.id === action?.id
        );

        let updatedList;
        if (filterExist) {
            updatedList = filteredBudgetDiscussionTypeList.filter(
                (filter) => filter?.id !== action?.id
            );
        } else {
            updatedList = [...filteredBudgetDiscussionTypeList, action];
        }

        updatedList.sort((a, b) => a?.id - b?.id);

        setFilteredBudgetDiscussionTypeList(updatedList);
    };

    const toggleProposalsOwnersFilter = (e) => {
        const propOwnerFilterId = e?.target?.value?.toString();
        let propOwnerFilter = proposalsOwnersList?.find(
            (f) => f?.id?.toString() === propOwnerFilterId
        );

        if (propOwnerFilter) {
            setProposalsOwnerFilter(propOwnerFilter);
        }
    };

    const resetFilters = () => {
        setFilteredBudgetDiscussionTypeList([]);
        handleCloseFilters();
        setShowAllActivated({
            is_activated: false,
            bd_type: null,
        });
        setProposalsOwnerFilter(defaultOwnerFilter);
    };

    useEffect(() => {
        if (budgetDiscussionTypeList.length == 0) fetchBudgetDiscussionTypes();
    }, []);

    useEffect(() => {
        if (showAllActivated?.is_activated) {
            setFilteredBudgetDiscussionTypeList([showAllActivated?.bd_type]);
        } else {
            setFilteredBudgetDiscussionTypeList([]);
        }
    }, [showAllActivated]);

    useEffect(() => {
        if (location.pathname.includes('propose')) {
            if (user?.user?.govtool_username) {
                setShowCreateBDDialog(true);
            } else {
                setOpenUsernameModal({ open: true, callBackFn: () => {} });
            }
        }
    }, [location.pathname]);

    const allEmptyMatchBudget =
        isAllProposalsListEmpty?.length > 0 &&
        filteredBudgetDiscussionTypeList?.length === 0
            ? budgetDiscussionTypeList.every((budget) =>
                  isAllProposalsListEmpty.some(
                      (empty) => empty?.id === budget?.id
                  )
              )
            : false;

    const allFilteredAreEmpty =
        isAllProposalsListEmpty?.length > 0 &&
        filteredBudgetDiscussionTypeList?.length > 0
            ? filteredBudgetDiscussionTypeList.every((filtered) =>
                  isAllProposalsListEmpty.some(
                      (empty) => empty?.id === filtered?.id
                  )
              )
            : false;

    const showNoProposals = allEmptyMatchBudget || allFilteredAreEmpty;

    useEffect(() => {
        console.log(proposalsOwnersList);
        if (user?.user?.id) {
            const myProposals = proposalsOwnersList?.find(
                (f) => f?.id === 'my-proposals'
            );
            if (myProposals) {
                myProposals.disabled = false;
                return;
            }
            proposalsOwnersList?.push({
                id: 'my-proposals',
                label: 'My Proposals',
                disabled: false,
            });
        }
        // else {
        //     if (proposalsOwnersList?.find((f) => f?.id === 'my-proposals')) {
        //         proposalsOwnersList = proposalsOwnersList.filter(
        //             (f) => f?.id === 'my-proposals'
        //         );
        //     }
        // }
    }, [user?.user?.id]);

    return (
        <Box sx={{ mt: 3 }}>
            <ScrollToTop step={showAllActivated?.is_activated} />
            <Box display={'flex'} flexDirection={'column'}>
                {!walletAPI?.address && (
                    <Typography
                        variant={isMobile ? 'title1' : 'headline3'}
                        component='h1'
                        sx={{ mb: isMobile ? 3.75 : 6 }}
                    >
                        Budget Proposals
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
                                    bd_type: null,
                                });
                            }}
                            data-testid='back-to-budget-proposals-button'
                        >
                            Back to Budget Proposals
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
                        style={{
                            maxHeight: '40px',
                            maxWidth: '350px',
                        }}
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
                                    setShowCreateBDDialog(true),
                                clearStates: clearStates,
                                addErrorAlert: addErrorAlert,
                                addSuccessAlert: addSuccessAlert,
                                addChangesSavedAlert:
                                    addChangesSavedAlert,
                            })
                        }
                        // startIcon={<IconPlusCircle fill='white' />}
                        data-testid='propose-a-budget-discussion-button'
                    >
                        Submit proposal for Cardano budget
                    </Button>
                    {checkShowValidation(
                        false,
                        walletAPI,
                        user
                    ) && (
                        <UserValidation
                            type='budget-proposal'
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
                                setBudgetDiscussionSearchText
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
                                    <Box mb={2}>
                                        <Typography sx={menuTitleSx}>
                                            Budget categories
                                        </Typography>
                                        {budgetDiscussionTypeList?.map(
                                            (ga, index) => (
                                                <MenuItem
                                                    key={`${ga?.attributes?.type_name}-${index}`}
                                                    selected={filteredBudgetDiscussionTypeList?.some(
                                                        (filter) =>
                                                            filter?.id ===
                                                            ga?.id
                                                    )}
                                                    sx={menuItemSx}
                                                    id={`${ga?.attributes?.type_name}-radio-wrapper`}
                                                    data-testid={
                                                        (ga?.attributes
                                                            ?.type_name ==
                                                        'None of these'
                                                            ? 'no-category'
                                                            : ga?.attributes?.type_name
                                                                  .replace(
                                                                      /\s+/g,
                                                                      '-'
                                                                  )
                                                                  .toLowerCase()) +
                                                        `-radio-wrapper`
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
                                                                checked={filteredBudgetDiscussionTypeList?.some(
                                                                    (
                                                                        filter
                                                                    ) =>
                                                                        filter?.id ===
                                                                        ga?.id
                                                                )}
                                                                id={`${ga?.attributes?.type_name}-radio`}
                                                                data-testid={
                                                                    (ga
                                                                        ?.attributes
                                                                        ?.type_name ==
                                                                    'None of these'
                                                                        ? 'no-category'
                                                                        : ga?.attributes?.type_name
                                                                              .replace(
                                                                                  /\s+/g,
                                                                                  '-'
                                                                              )
                                                                              .toLowerCase()) +
                                                                    `-radio`
                                                                }
                                                            />
                                                        }
                                                        label={
                                                            ga
                                                                ?.attributes
                                                                ?.type_name ===
                                                            'None of these'
                                                                ? 'No category'
                                                                : ga
                                                                      ?.attributes
                                                                      ?.type_name
                                                        }
                                                    />
                                                </MenuItem>
                                            )
                                        )}
                                    </Box>
                                )}
                                <Typography sx={menuTitleSx}>
                                    Proposals owners
                                </Typography>

                                {proposalsOwnersList?.map(
                                    (ga, index) => (
                                        <MenuItem
                                            key={`${ga?.id}-${index}`}
                                            selected={
                                                proposalsOwnerFilter?.id ===
                                                ga?.id
                                            }
                                            id={`${ga?.id}-radio-wrapper`}
                                            data-testid={
                                                ga?.label
                                                    ?.replace(
                                                        /\s+/g,
                                                        '-'
                                                    )
                                                    ?.toLowerCase() +
                                                `-radio-wrapper`
                                            }
                                            onClick={
                                                toggleProposalsOwnersFilter
                                            }
                                            sx={{
                                                ...menuItemSx,
                                                width: '100%',
                                            }}
                                        >
                                            <FormControlLabel
                                                name='owner-filter'
                                                control={
                                                    <Radio
                                                        checked={
                                                            proposalsOwnerFilter?.id ===
                                                            ga?.id
                                                        }
                                                        disabled={
                                                            ga?.disabled
                                                        }
                                                    />
                                                }
                                                id={`${ga?.label}-radio`}
                                                data-testid={
                                                    ga?.label
                                                        ?.replace(
                                                            /\s+/g,
                                                            '-'
                                                        )
                                                        ?.toLowerCase() +
                                                    `-radio`
                                                }
                                                sx={{
                                                    width: '100%',
                                                    marginRight: 0,
                                                    marginLeft: 0,
                                                }}
                                                value={ga?.id}
                                                label={
                                                    <Typography
                                                        data-testid={`${ga?.label}-owner-filter-option`}
                                                        color={
                                                            'textBlack'
                                                        }
                                                        variant='body1'
                                                        fontWeight={400}
                                                        sx={{
                                                            width: '100%',
                                                            overflowX:
                                                                'hidden',
                                                            textOverflow:
                                                                'ellipsis',
                                                        }}
                                                    >
                                                        {ga?.label}
                                                    </Typography>
                                                }
                                            />
                                        </MenuItem>
                                    )
                                )}

                                <MenuItem
                                    onClick={() => resetFilters()}
                                    data-testid='reset-filters'
                                    sx={{ ...menuItemSx, mt: 1 }}
                                >
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
                                ? 'Newest first'
                                : 'Oldest first'}
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
                {(filteredBudgetDiscussionTypeList?.length > 0
                    ? filteredBudgetDiscussionTypeList
                    : budgetDiscussionTypeList
                )?.map((item, index) => (
                    <Box
                        key={`${item?.attributes?.type_name}-${index}`}
                    >
                        <BudgetDiscussionsList
                            currentBudgetDiscussionType={item}
                            searchText={budgetDiscussionSearchText?.trim()}
                            sortOption={sortType}
                            statusList={filteredBudgetDiscussionStatusList}
                            setShowAllActivated={setShowAllActivated}
                            showAllActivated={showAllActivated}
                            isAllProposalsListEmpty={isAllProposalsListEmpty}
                            setIsAllProposalsListEmpty={
                                setIsAllProposalsListEmpty
                            }
                            filteredBudgetDiscussionTypeList={
                                filteredBudgetDiscussionTypeList
                            }
                            proposalOwnerFilter={proposalsOwnerFilter}
                        />
                    </Box>
                ))}
            </Box>

            {showNoProposals ? (
                <Card
                    sx={{
                        backgroundColor: 'rgba(255, 255, 255, 0.3)',
                        borderRadius: '20px',
                        boxShadow: '0px 4px 15px 0px #DDE3F5',
                        my: 3,
                    }}
                >
                    <CardContent>
                        <Stack
                            display={'flex'}
                            direction={'column'}
                            alignItems={'center'}
                            justifyContent={'center'}
                            gap={1}
                        >
                            <Typography
                                variant='title2'
                                component='h6'
                                color='textBlack'
                                fontWeight={600}
                            >
                                No budget discussions found
                            </Typography>
                            <Typography
                                variant='body1'
                                fontWeight={400}
                                color='textBlack'
                            >
                                Please try a different search
                            </Typography>
                        </Stack>
                    </CardContent>
                </Card>
            ) : null}

            {showCreateBDDialog && (
                <CreateBudgetDiscussionDialog
                    open={showCreateBDDialog}
                    onClose={() => setShowCreateBDDialog(false)}
                />
            )}
        </Box>
    );
};

export default ProposedBudgetDiscussion;
