'use client';

import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import {
    Box,
    Card,
    CardContent,
    Grid,
    IconButton,
    Stack,
    useMediaQuery,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useRef, useState, useCallback, useMemo } from 'react';
import Slider from 'react-slick';
import { ProposalCard } from '..';
import { useDebounce } from '../..//lib/hooks';
import { getProposals } from '../../lib/api';
import { settings } from '../../lib/carouselSettings';
import { useTheme } from '@emotion/react';

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

const ProposalsList = ({
    governanceAction,
    searchText = '',
    sortType = { fieldId: 'createdAt', type: 'DESC', title: 'Newest' },
    isDraft = false,
    statusList = [],
    startEdittinButtonClick = false,
    setShowAllActivated = false,
    showAllActivated = false,
}) => {
    const theme = useTheme();
    const sliderRef = useRef(null);
    const observer = useRef();

    const [showAll, setShowAll] = useState(false);
    const [proposalsList, setProposalsList] = useState([]);
    const [pageCount, setPageCount] = useState(0);
    const [currentPage, setCurrentPage] = useState(1);
    const [mounted, setMounted] = useState(false);
    // const debouncedSearchValue = useDebounce(searchText.trim());
    const [shouldRefresh, setShouldRefresh] = useState(false);
    const isXs = useMediaQuery(theme.breakpoints.down('md'));
    const isSm = useMediaQuery(theme.breakpoints.only('md'));
    const isMd = useMediaQuery(theme.breakpoints.only('lg'));
    const isLg = useMediaQuery(theme.breakpoints.only('xl'));

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

    const fetchProposals = async (reset = true, page) => {
        console.log('Fetching proposals');
        const haveSubmittedFilter = statusList?.some(
            (filter) => filter === 'submitted'
        );

        try {
            let query = '';
            if (isDraft) {
                if (statusList?.length === 0 || statusList?.length === 2) {
                    query = `filters[$and][2][is_draft]=true&pagination[page]=${page}&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content`;
                } else {
                    const isSubmitted = haveSubmittedFilter ? 'true' : 'false';
                    query = `filters[$and][2][is_draft]=true&filters[$and][3][prop_submitted]=${isSubmitted}&pagination[page]=${page}&pagination[pageSize]=25&sort[createdAt]=desc&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content`;
                }
            } else {
                if (statusList?.length === 0 || statusList?.length === 2) {
                    query = `filters[$and][0][gov_action_type_id]=${
                        governanceAction?.id
                    }&filters[$and][1][prop_name][$containsi]=${
                        searchText || ''
                    }&pagination[page]=${page}&pagination[pageSize]=25&sort[${sortType.fieldId}]=${sortType.type}&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal`;
                } else {
                    const isSubmitted = haveSubmittedFilter ? 'true' : 'false';
                    query = `filters[$and][0][gov_action_type_id]=${
                        governanceAction?.id
                    }&filters[$and][1][prop_name][$containsi]=${
                        searchText || ''
                    }&filters[$and][2][prop_submitted]=${isSubmitted}&pagination[page]=${page}&pagination[pageSize]=25&sort[${sortType.fieldId}]=${sortType.type}&populate[0]=proposal_links&populate[1]=proposal_withdrawals&populate[2]=proposal_constitution_content&populate[3]=proposal`;
                }
            }
            const { proposals, pgCount } = await getProposals(query);
            if (!proposals) return;

            if (reset) {
                setProposalsList(proposals);
            } else {
                setProposalsList((prev) => [...prev, ...proposals]);
            }
            setPageCount(pgCount);
        } catch (error) {
            console.error(error);
        }
    };

    // Memoize sortType and statusList dependencies to prevent infinite re-renders
    const sortTypeString = useMemo(() => JSON.stringify(sortType), [sortType]);
    const statusListString = useMemo(
        () => JSON.stringify(statusList),
        [statusList]
    );

    useEffect(() => {
        if (!mounted) {
            setMounted(true);
        } else {
            fetchProposals(true, 1);
            setCurrentPage(1);
        }
    }, [
        mounted,
        // debouncedSearchValue,
        searchText,
        sortTypeString,
        isDraft ? null : statusListString,
        showAllActivated,
    ]);

    useEffect(() => {
        if (shouldRefresh) {
            fetchProposals(true, 1);
            setShouldRefresh(false);
        }
    }, [shouldRefresh]);

    const lastProposalRef = useCallback(
        (node) => {
            if (observer.current) observer.current.disconnect();
            observer.current = new window.IntersectionObserver((entries) => {
                if (
                    entries[0].isIntersecting &&
                    currentPage < pageCount &&
                    proposalsList.length > 0
                ) {
                    fetchProposals(false, currentPage + 1);
                    setCurrentPage((prev) => prev + 1);
                }
            });
            if (node) observer.current.observe(node);
        },
        [currentPage, pageCount, proposalsList.length]
    );

    return isDraft && proposalsList?.length === 0 ? null : (
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
                        {isDraft
                            ? 'Unfinished Drafts'
                            : governanceAction?.attributes
                                  ?.gov_action_type_name}
                    </Typography>
                    {proposalsList?.length > 0 &&
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
                                            gov_action_type: governanceAction,
                                        }));
                                    }
                                }}
                                data-testid={
                                    governanceAction?.attributes?.gov_action_type_name
                                        .replace(/\s+/g, '-')
                                        .toLowerCase() + '-show-all-button'
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
                    proposalsList?.length > 0 && (
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
            {proposalsList?.length > 0 ? (
                (
                    showAllActivated ? showAllActivated?.is_activated : showAll
                ) ? (
                    <Box>
                        <Grid container spacing={2.5} paddingY={2}>
                            {proposalsList?.map((proposal, index) => {
                                if (index === proposalsList.length - 1) {
                                    return (
                                        <Grid
                                            item
                                            key={index}
                                            xxs={12}
                                            md={6}
                                            lg={4}
                                            ref={lastProposalRef}
                                        >
                                            <ProposalCard
                                                proposal={proposal}
                                                startEdittinButtonClick={
                                                    startEdittinButtonClick
                                                }
                                                setShouldRefresh={
                                                    setShouldRefresh
                                                }
                                            />
                                        </Grid>
                                    );
                                } else {
                                    return (
                                        <Grid
                                            item
                                            key={index}
                                            xxs={12}
                                            md={6}
                                            lg={4}
                                        >
                                            <ProposalCard
                                                proposal={proposal}
                                                startEdittinButtonClick={
                                                    startEdittinButtonClick
                                                }
                                                setShouldRefresh={
                                                    setShouldRefresh
                                                }
                                            />
                                        </Grid>
                                    );
                                }
                            })}
                        </Grid>
                    </Box>
                ) : (
                    <Box py={2}>
                        <Slider ref={sliderRef} {...settings}>
                            {proposalsList?.map((proposal, index) => (
                                <Box key={index} height={'100%'}>
                                    <ProposalCard
                                        proposal={proposal}
                                        startEdittinButtonClick={
                                            startEdittinButtonClick
                                        }
                                        setShouldRefresh={setShouldRefresh}
                                    />
                                </Box>
                            ))}

                            {boxesToRender}
                        </Slider>
                    </Box>
                )
            ) : (
                <Card
                    sx={{
                        backgroundColor: 'rgba(255, 255, 255, 0.3)',
                        borderRadius: '20px',
                        boxShadow: '0px 4px 15px 0px #DDE3F5',
                        my: 2,
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
                                No proposals found
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
            )}
        </Box>
    );
};

export default ProposalsList;
