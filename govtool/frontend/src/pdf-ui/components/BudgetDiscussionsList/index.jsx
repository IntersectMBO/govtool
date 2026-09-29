'use client';

import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {
    Box,
    Card,
    CardContent,
    Grid,
    IconButton,
    Stack,
    alpha,
    useMediaQuery,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useRef, useState, useCallback, useMemo } from 'react';
import Slider from 'react-slick';
import { useDebounce } from '../..//lib/hooks';
import { getBudgetDiscussionDrafts, getBudgetDiscussions } from '../../lib/api';
import { settings } from '../../lib/carouselSettings';
import { useTheme } from '@emotion/react';
import { BudgetDiscussionsCard } from '..';
import { useAppContext } from '../../context/context';

// The "Show all" button and the carousel arrows of GovTool's Slider and
// SliderArrow.
const showAllButtonSx = {
    border: '1px solid',
    borderColor: 'lightBlue',
    bgcolor: 'arcticWhite',
    boxShadow: 'none',
    color: 'primaryBlue',
    minWidth: 93,
    '&:hover': { bgcolor: 'arcticWhite', boxShadow: 'none' },
};

const sliderArrowSx = {
    width: 44,
    height: 44,
    border: '1px solid',
    borderColor: 'lightBlue',
    bgcolor: 'arcticWhite',
    transition: '0.3s',
    '&:hover': { bgcolor: 'arcticWhite', boxShadow: 2 },
};

const BudgetDiscussionsList = ({
    currentBudgetDiscussionType = null,
    searchText = '',
    sortOption = { fieldId: 'createdAt', type: 'DESC', title: 'Newest' },
    isDraft = false,
    statusList = [],
    startEdittinButtonClick = false,
    startEdittingDraft,
    setShowAllActivated = false,
    showAllActivated = false,
    isAllProposalsListEmpty,
    setIsAllProposalsListEmpty,
    filteredBudgetDiscussionTypeList,
    proposalOwnerFilter = null,
}) => {
    const theme = useTheme();
    const sliderRef = useRef(null);
    const observer = useRef();

    const [showAll, setShowAll] = useState(false);
    const [budgetDiscussionList, setBudgetDiscussionList] = useState([]);
    const [pageCount, setPageCount] = useState(0);
    const [currentPage, setCurrentPage] = useState(1);
    const [mounted, setMounted] = useState(false);
    const debouncedSearchValue = searchText;
    const [shouldRefresh, setShouldRefresh] = useState(false);
    const isXs = useMediaQuery(theme.breakpoints.down('md'));
    const isSm = useMediaQuery(theme.breakpoints.only('md'));
    const isMd = useMediaQuery(theme.breakpoints.only('lg'));
    const isLg = useMediaQuery(theme.breakpoints.only('xl'));

    const { user } = useAppContext();

    let extraBoxes = 0;

    if (isXs) {
        extraBoxes = 0;
    } else if (isSm) {
        extraBoxes = 1;
    } else if (isMd) {
        extraBoxes = 2;
    } else if (isLg) {
        extraBoxes = 2;
    } else {
        extraBoxes = 2;
    }

    const boxesToRender = Array.from({ length: extraBoxes }, (_, index) => (
        <Box key={`extra-${index}`} height={'100%'} />
    ));

    const fetchBudgetDiscussions = useCallback(
        async (reset = true, page) => {
            try {
                if (isDraft) {
                    let bdlist = await getBudgetDiscussionDrafts();
                    setBudgetDiscussionList(bdlist.data);
                } else {
                    let query = `filters[$and][0][is_active]=true&filters[$and][1][bd_psapb][type_name][id]=${
                        currentBudgetDiscussionType?.id
                    }&filters[$and][2][bd_proposal_detail][proposal_name][$containsi]=${
                        debouncedSearchValue || ''
                    }${proposalOwnerFilter?.id === 'all-proposals' ? '' : user?.user?.id ? '&filters[$and][3][creator]=' + user?.user?.id : ''}&pagination[page]=${page}&pagination[pageSize]=25&sort[${sortOption.fieldId}]=${
                        sortOption.type
                    }&populate[0]=bd_costing&populate[1]=bd_psapb.type_name&populate[2]=bd_proposal_detail&populate[3]=creator`;
                    const { budgetDiscussions, pgCount, total } =
                        await getBudgetDiscussions(query);

                    if (!budgetDiscussions) return;
                    if (reset) {
                        setBudgetDiscussionList(budgetDiscussions);
                    } else {
                        setBudgetDiscussionList((prev) => [
                            ...prev,
                            ...budgetDiscussions,
                        ]);
                    }
                    setPageCount(pgCount);
                }
            } catch (error) {
                console.error(error);
            }
        },
        [
            isDraft,
            currentBudgetDiscussionType?.id,
            debouncedSearchValue,
            proposalOwnerFilter?.id,
            user?.user?.id,
            sortOption.fieldId,
            sortOption.type,
        ]
    );

    // Separate useEffect for drafts - Infinite calls issue
    // This effect only runs when isDraft is true
    useEffect(() => {
        if (mounted && isDraft) {
            fetchBudgetDiscussions(true, 1);
            setCurrentPage(1);
        }
    }, [mounted, isDraft]);

    useEffect(() => {
        if (mounted && !isDraft) {
            fetchBudgetDiscussions(true, 1);
            setCurrentPage(1);
        }
    }, [
        mounted,
        isDraft,
        debouncedSearchValue,
        sortOption.fieldId,
        sortOption.type,
        statusList,
        showAllActivated,
        currentBudgetDiscussionType?.id,
        proposalOwnerFilter?.id,
        fetchBudgetDiscussions,
    ]);

    // Mount effect
    useEffect(() => {
        setMounted(true);
    }, []);

    useEffect(() => {
        if (shouldRefresh) {
            fetchBudgetDiscussions(true, 1);
            setShouldRefresh(false);
        }
    }, [shouldRefresh, fetchBudgetDiscussions]);

    useEffect(() => {
        if (!isDraft) {
            if (budgetDiscussionList?.length === 0) {
                let emptyProposals =
                    isAllProposalsListEmpty?.length > 0
                        ? [...isAllProposalsListEmpty]
                        : [];

                // check if category id is inside this list
                const alreadyInList = emptyProposals.some(
                    (item) => item?.id === currentBudgetDiscussionType?.id
                );
                if (!alreadyInList) {
                    emptyProposals.push({
                        id: currentBudgetDiscussionType?.id,
                        name: currentBudgetDiscussionType?.attributes
                            ?.type_name,
                    });
                    setIsAllProposalsListEmpty(emptyProposals);
                }
            } else {
                let emptyProposals =
                    isAllProposalsListEmpty?.length > 0
                        ? [...isAllProposalsListEmpty]
                        : [];

                // check if category id is inside this list
                const alreadyInList = emptyProposals.some(
                    (item) => item?.id === currentBudgetDiscussionType?.id
                );

                // empty it from the list
                if (alreadyInList) {
                    emptyProposals = emptyProposals.filter(
                        (item) => item?.id !== currentBudgetDiscussionType?.id
                    );
                    setIsAllProposalsListEmpty(emptyProposals);
                }
            }
        }
    }, [
        debouncedSearchValue,
        budgetDiscussionList?.length,
        isAllProposalsListEmpty?.length,
        filteredBudgetDiscussionTypeList?.length,
        isDraft,
        currentBudgetDiscussionType?.id,
        currentBudgetDiscussionType?.attributes?.type_name,
        setIsAllProposalsListEmpty,
    ]);

    const lastBudgetProposalRef = useCallback(
        (node) => {
            if (observer.current) observer.current.disconnect();
            observer.current = new window.IntersectionObserver((entries) => {
                if (
                    entries[0].isIntersecting &&
                    currentPage < pageCount &&
                    budgetDiscussionList.length > 0
                ) {
                    fetchBudgetDiscussions(false, currentPage + 1);
                    setCurrentPage((prev) => prev + 1);
                }
            });
            if (node) observer.current.observe(node);
        },
        [currentPage, pageCount, budgetDiscussionList.length]
    );

    return budgetDiscussionList?.length === 0 ? null : (
        <Box overflow={'visible'}>
            <Box
                sx={{
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                    gap: '10px',
                    mb: 1.5,
                    minHeight: '46px',
                }}
            >
                <Box
                    sx={{
                        display: 'flex',
                        alignItems: 'center',
                        gap: 2,
                    }}
                >
                    <Typography
                        variant='title2'
                        component='h2'
                        color='textBlack'
                    >
                        {isDraft ? 'Unfinished Drafts' : ''}
                        {currentBudgetDiscussionType?.attributes?.type_name ==
                        'None of these'
                            ? 'No category'
                            : currentBudgetDiscussionType?.attributes
                                  ?.type_name}
                    </Typography>
                    {budgetDiscussionList?.length > 0 &&
                        (setShowAllActivated
                            ? !showAllActivated?.is_activated
                            : true) && (
                            <Button
                                variant='contained'
                                size='medium'
                                sx={showAllButtonSx}
                                onClick={() => {
                                    setShowAll((prev) => !prev);
                                    if (setShowAllActivated) {
                                        setShowAllActivated(() => ({
                                            is_activated: true,
                                            bd_type:
                                                currentBudgetDiscussionType,
                                        }));
                                    }
                                }}
                                data-testid={
                                    isDraft
                                        ? 'draft-show-all-button'
                                        : currentBudgetDiscussionType
                                                ?.attributes?.type_name ==
                                            'None of these'
                                          ? 'no-category-show-all-button'
                                          : currentBudgetDiscussionType?.attributes?.type_name
                                                .replace(/\s+/g, '-')
                                                .toLowerCase() +
                                            '-show-all-button'
                                }
                            >
                                {setShowAllActivated
                                    ? setShowAllActivated?.is_activated
                                        ? 'Show less'
                                        : 'Show all'
                                    : showAll
                                      ? 'Show less'
                                      : 'Show all'}
                            </Button>
                        )}
                </Box>

                {(setShowAllActivated
                    ? !showAllActivated?.is_activated
                    : !showAll) &&
                    budgetDiscussionList?.length > 0 && (
                        <Box display={'flex'} gap={'4px'}>
                            <IconButton
                                onClick={() => sliderRef.current.slickPrev()}
                                sx={sliderArrowSx}
                            >
                                <ChevronRightIcon
                                    sx={{
                                        color: 'primaryBlue',
                                        transform: 'rotate(180deg)',
                                    }}
                                />
                            </IconButton>
                            <IconButton
                                onClick={() => sliderRef.current.slickNext()}
                                sx={sliderArrowSx}
                            >
                                <ChevronRightIcon
                                    sx={{ color: 'primaryBlue' }}
                                />
                            </IconButton>
                        </Box>
                    )}
            </Box>
            {budgetDiscussionList?.length > 0 ? (
                (
                    showAllActivated ? showAllActivated?.is_activated : showAll
                ) ? (
                    <Box>
                        <Grid container spacing={2.5} paddingY={2}>
                            {budgetDiscussionList?.map((bd, index) => {
                                const isLast =
                                    index === budgetDiscussionList.length - 1;
                                return (
                                    <Grid
                                        item
                                        key={index}
                                        xxs={12}
                                        md={6}
                                        lg={4}
                                        ref={
                                            isLast
                                                ? lastBudgetProposalRef
                                                : null
                                        }
                                    >
                                        <BudgetDiscussionsCard
                                            budgetDiscussion={bd}
                                            isDraft={isDraft}
                                            startEdittingDraft={
                                                startEdittingDraft
                                            }
                                            startEdittinButtonClick={
                                                startEdittinButtonClick
                                            }
                                        />
                                    </Grid>
                                );
                            })}
                        </Grid>

                        {/* {currentPage < pageCount && (
                            <Box
                                marginY={2}
                                display={'flex'}
                                justifyContent={'flex-end'}
                            >
                                <Button
                                    onClick={() => {
                                        fetchBudgetDiscussions(
                                            false,
                                            currentPage + 1
                                        );
                                        setCurrentPage((prev) => prev + 1);
                                    }}
                                >
                                    Load more
                                </Button>
                            </Box>
                        )} */}
                    </Box>
                ) : (
                    <Box py={2}>
                        <Slider ref={sliderRef} {...settings}>
                            {budgetDiscussionList?.map((bd, index) => (
                                <Box key={index} height={'100%'}>
                                    <BudgetDiscussionsCard
                                        budgetDiscussion={bd}
                                        isDraft={isDraft}
                                        startEdittingDraft={startEdittingDraft}
                                        startEdittinButtonClick={
                                            startEdittinButtonClick
                                        }
                                    />
                                </Box>
                            ))}

                            {boxesToRender}
                        </Slider>
                    </Box>
                )
            ) : null}
        </Box>
    );
};

export default BudgetDiscussionsList;
