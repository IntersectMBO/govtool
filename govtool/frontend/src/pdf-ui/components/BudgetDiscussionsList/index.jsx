'use client';

import ChevronRightIcon from '@mui/icons-material/ChevronRight';
import { Box, Grid, IconButton, useMediaQuery } from '@mui/material';
import { Button, Typography } from '@atoms';
import { useRef } from 'react';
import Slider from 'react-slick';
import { settings } from '../../lib/carouselSettings';
import { useTheme } from '@emotion/react';
import BudgetDiscussionsCard from '../BudgetDiscussionCard';
import { categoryLabel, categorySlug } from '../../lib/budgetArchive';

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

// One budget category of the archive: a carousel, or every card in a grid
// when `showAll` is set. The items arrive filtered and sorted.
const BudgetDiscussionsList = ({
    category,
    items = [],
    showAll = false,
    onShowAll,
}) => {
    const theme = useTheme();
    const sliderRef = useRef(null);
    const isXs = useMediaQuery(theme.breakpoints.down('md'));
    const isSm = useMediaQuery(theme.breakpoints.only('md'));

    // Empty slides keep a short carousel's cards at their normal width.
    let extraBoxes = 2;
    if (isXs) extraBoxes = 0;
    else if (isSm) extraBoxes = 1;

    const boxesToRender = Array.from({ length: extraBoxes }, (_, index) => (
        <Box key={`extra-${index}`} height={'100%'} />
    ));

    if (items.length === 0) return null;

    const name = category?.attributes?.type_name;

    return (
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
                <Box sx={{ display: 'flex', alignItems: 'center', gap: 2 }}>
                    <Typography
                        variant='title2'
                        component='h2'
                        color='textBlack'
                    >
                        {categoryLabel(name)}
                    </Typography>
                    {!showAll && (
                        <Button
                            variant='contained'
                            size='medium'
                            sx={showAllButtonSx}
                            onClick={onShowAll}
                            data-testid={`${categorySlug(name)}-show-all-button`}
                        >
                            Show all
                        </Button>
                    )}
                </Box>

                {!showAll && (
                    <Box display={'flex'} gap={'4px'}>
                        <IconButton
                            onClick={() => sliderRef.current?.slickPrev()}
                            sx={sliderArrowSx}
                            aria-label='previous'
                        >
                            <ChevronRightIcon
                                sx={{
                                    color: 'primaryBlue',
                                    transform: 'rotate(180deg)',
                                }}
                            />
                        </IconButton>
                        <IconButton
                            onClick={() => sliderRef.current?.slickNext()}
                            sx={sliderArrowSx}
                            aria-label='next'
                        >
                            <ChevronRightIcon sx={{ color: 'primaryBlue' }} />
                        </IconButton>
                    </Box>
                )}
            </Box>
            {showAll ? (
                <Grid container spacing={2.5} paddingY={2}>
                    {items.map((bd) => (
                        <Grid item key={bd.attributes.master_id} xxs={12} md={6} lg={4}>
                            <BudgetDiscussionsCard budgetDiscussion={bd} />
                        </Grid>
                    ))}
                </Grid>
            ) : (
                <Box py={2}>
                    <Slider ref={sliderRef} {...settings}>
                        {items.map((bd) => (
                            <Box key={bd.attributes.master_id} height={'100%'}>
                                <BudgetDiscussionsCard budgetDiscussion={bd} />
                            </Box>
                        ))}
                        {boxesToRender}
                    </Slider>
                </Box>
            )}
        </Box>
    );
};

export default BudgetDiscussionsList;
