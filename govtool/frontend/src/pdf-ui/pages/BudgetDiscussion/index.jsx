'use client';

import ArrowBackIosIcon from '@mui/icons-material/ArrowBackIos';
import { ICONS } from '@/consts/icons';
import {
    Box,
    Checkbox,
    CircularProgress,
    FormControlLabel,
    Menu,
    MenuItem,
    Card,
    CardContent,
    Stack,
} from '@mui/material';
import { Button, Typography } from '@atoms';
import { useEffect, useMemo, useState } from 'react';
import { useNavigate } from 'react-router';
import BudgetDiscussionsList from '../../components/BudgetDiscussionsList';
import SearchInput from '../../components/SearchInput';
import { useScreenDimension } from '@/hooks/useScreenDimension';
import { ScrollToTop } from '../../lib/hooks';
import { primaryBlue } from '@/consts/colors';
import {
    SORT_OPTIONS,
    categoryId,
    categoryLabel,
    categorySlug,
    filterArchiveItems,
    findCategoryBySlug,
    getArchiveList,
} from '../../lib/budgetArchive';

const LIST_PATH = '/budget_discussion';

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

const menuPlacement = {
    transformOrigin: { horizontal: 'right', vertical: 'top' },
    anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
};

const EmptyState = ({ title, message }) => (
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
                    {title}
                </Typography>
                <Typography variant='body1' fontWeight={400} color='textBlack'>
                    {message}
                </Typography>
            </Stack>
        </CardContent>
    </Card>
);

// The read-only list of the 2025 budget proposals. `category` is the slug
// of /budget_discussion/category/:category, which shows that category in
// full; without it every category is a carousel.
const ProposedBudgetDiscussion = ({ category = null, showTitle = true }) => {
    const navigate = useNavigate();
    const { isMobile } = useScreenDimension();

    const [archive, setArchive] = useState(null);
    const [loadError, setLoadError] = useState(false);
    const [searchText, setSearchText] = useState('');
    const [sortOption, setSortOption] = useState(SORT_OPTIONS[0]);
    const [selectedCategoryIds, setSelectedCategoryIds] = useState([]);
    const [filtersAnchorEl, setFiltersAnchorEl] = useState(null);
    const [sortAnchorEl, setSortAnchorEl] = useState(null);

    const openFilters = Boolean(filtersAnchorEl);
    const openSort = Boolean(sortAnchorEl);

    useEffect(() => {
        let cancelled = false;
        getArchiveList()
            .then((data) => {
                if (!cancelled) setArchive(data);
            })
            .catch((error) => {
                console.error(error);
                if (!cancelled) setLoadError(true);
            });
        return () => {
            cancelled = true;
        };
    }, []);

    const categories = archive?.categories ?? [];
    const routeCategory = category
        ? findCategoryBySlug(categories, category)
        : null;

    // An unknown category slug falls back to the whole list.
    useEffect(() => {
        if (archive && category && !routeCategory) {
            navigate(LIST_PATH, { replace: true });
        }
    }, [archive, category, routeCategory, navigate]);

    const shownCategories = useMemo(() => {
        if (routeCategory) return [routeCategory];
        if (selectedCategoryIds.length === 0) return categories;
        return categories.filter((c) => selectedCategoryIds.includes(c.id));
    }, [categories, routeCategory, selectedCategoryIds]);

    const filteredItems = useMemo(
        () =>
            filterArchiveItems(archive?.items, {
                searchText,
                categoryIds: shownCategories.map((c) => c.id),
                sortId: sortOption.id,
            }),
        [archive, searchText, sortOption.id, shownCategories]
    );

    const toggleCategoryFilter = (id) =>
        setSelectedCategoryIds((prev) =>
            prev.includes(id)
                ? prev.filter((existing) => existing !== id)
                : [...prev, id].sort((a, b) => a - b)
        );

    const resetFilters = () => {
        setSelectedCategoryIds([]);
        setFiltersAnchorEl(null);
    };

    const renderContent = () => {
        if (loadError) {
            return (
                <EmptyState
                    title='The archive could not be loaded'
                    message='Please reload the page to try again'
                />
            );
        }
        if (!archive) {
            return (
                <Box display='flex' justifyContent='center' mt={10}>
                    <CircularProgress />
                </Box>
            );
        }
        return (
            <>
                <Box
                    sx={{
                        display: 'flex',
                        flexDirection: 'column',
                        gap: 5,
                        mt: isMobile ? 7.5 : 10,
                    }}
                >
                    {shownCategories.map((c) => (
                        <BudgetDiscussionsList
                            key={c.id}
                            category={c}
                            items={filteredItems.filter(
                                (item) => categoryId(item) === c.id
                            )}
                            showAll={Boolean(routeCategory)}
                            onShowAll={() =>
                                navigate(
                                    `${LIST_PATH}/category/${categorySlug(
                                        c?.attributes?.type_name
                                    )}`
                                )
                            }
                        />
                    ))}
                </Box>
                {filteredItems.length === 0 && (
                    <EmptyState
                        title='No budget proposals found'
                        message='Please try a different search'
                    />
                )}
            </>
        );
    };

    return (
        <Box sx={{ mt: 3 }}>
            <ScrollToTop step={category} />
            <Box display={'flex'} flexDirection={'column'}>
                {showTitle && (
                    <Typography
                        variant={isMobile ? 'title1' : 'headline3'}
                        component='h1'
                        sx={{ mb: isMobile ? 3.75 : 6 }}
                    >
                        2025 Budget Proposals
                    </Typography>
                )}

                {routeCategory && (
                    <Box mb={2}>
                        <Button
                            variant='text'
                            size='medium'
                            startIcon={<ArrowBackIosIcon color='primary' />}
                            onClick={() => navigate(LIST_PATH)}
                            data-testid='back-to-budget-proposals-button'
                        >
                            Back to Budget Proposals
                        </Button>
                    </Box>
                )}

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
                            onDebouncedChange={setSearchText}
                            placeholder='Search by name or proposer...'
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
                        {!routeCategory && (
                            <>
                                <Button
                                    variant='text'
                                    onClick={(e) =>
                                        setFiltersAnchorEl(e.currentTarget)
                                    }
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
                                    onClose={() => setFiltersAnchorEl(null)}
                                    MenuListProps={{
                                        'aria-labelledby': 'filters-button',
                                    }}
                                    slotProps={{
                                        paper: { elevation: 0, sx: menuPaperSx },
                                    }}
                                    {...menuPlacement}
                                >
                                    <Box mb={2}>
                                        <Typography sx={menuTitleSx}>
                                            Budget categories
                                        </Typography>
                                        {categories.map((c) => {
                                            const name =
                                                c?.attributes?.type_name;
                                            const slug = categorySlug(name);
                                            const checked =
                                                selectedCategoryIds.includes(
                                                    c.id
                                                );
                                            return (
                                                <MenuItem
                                                    key={c.id}
                                                    selected={checked}
                                                    sx={menuItemSx}
                                                    data-testid={`${slug}-radio-wrapper`}
                                                >
                                                    <FormControlLabel
                                                        sx={{
                                                            m: 0,
                                                            width: '100%',
                                                        }}
                                                        control={
                                                            <Checkbox
                                                                onChange={() =>
                                                                    toggleCategoryFilter(
                                                                        c.id
                                                                    )
                                                                }
                                                                checked={
                                                                    checked
                                                                }
                                                                data-testid={`${slug}-radio`}
                                                            />
                                                        }
                                                        label={categoryLabel(
                                                            name
                                                        )}
                                                    />
                                                </MenuItem>
                                            );
                                        })}
                                    </Box>
                                    <MenuItem
                                        onClick={resetFilters}
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
                                </Menu>
                            </>
                        )}
                        <Button
                            variant='text'
                            onClick={(e) => setSortAnchorEl(e.currentTarget)}
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
                            Sort: {sortOption.title}
                        </Button>
                        <Menu
                            id='sort-menu'
                            anchorEl={sortAnchorEl}
                            open={openSort}
                            onClose={() => setSortAnchorEl(null)}
                            MenuListProps={{ 'aria-labelledby': 'sort-button' }}
                            slotProps={{
                                paper: { elevation: 0, sx: menuPaperSx },
                            }}
                            {...menuPlacement}
                        >
                            {SORT_OPTIONS.map((sort) => (
                                <MenuItem
                                    key={sort.id}
                                    selected={sort.id === sortOption.id}
                                    id={sort.title}
                                    data-testid={`${sort.title}-sort-option`}
                                    onClick={() => {
                                        setSortOption(sort);
                                        setSortAnchorEl(null);
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
                        </Menu>
                    </Box>
                </Box>
            </Box>

            {renderContent()}
        </Box>
    );
};

export default ProposedBudgetDiscussion;
