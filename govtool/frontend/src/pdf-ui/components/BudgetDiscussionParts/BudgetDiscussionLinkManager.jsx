import { useEffect, useState } from 'react';
import {
    IconPlus,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { Box, IconButton } from '@mui/material';
import { Button, Typography } from '@atoms';
import { useTheme } from '@mui/material/styles';

import { isValidURLFormat } from '../../lib/utils';
import { PdfInput } from '../PdfFields';

const BudgetDiscussionLinkManager = ({
    maxLinks = 20,
    budgetDiscussionData,
    setBudgetDiscussionData,
    setLinksData,
    errors,
    setErrors,
}) => {
    const theme = useTheme();
    const [linksErrors, setLinksErrors] = useState({});

    useEffect(() => {
        if (
            budgetDiscussionData.bd_further_information?.proposal_links ===
            undefined
        ) {
            let links = [{ prop_link: '' }, { prop_link: '' }];
            setBudgetDiscussionData({
                ...budgetDiscussionData,
                bd_further_information: {
                    ...budgetDiscussionData.bd_further_information,
                    proposal_links: links,
                },
            });
        }
    }, []);

    useEffect(() => {
        setErrors((prev) => ({ ...prev, linkErrors: { ...linksErrors } }));
    }, [linksErrors]);

    const updateProposalLinks = (newLinks) => {
        setBudgetDiscussionData((prev) => ({
            ...prev,
            bd_further_information: {
                ...prev.bd_further_information,
                proposal_links: newLinks,
            },
        }));
    };

    const handleLinkChange = (index, field, value) => {
        const newLinks =
            budgetDiscussionData.bd_further_information?.proposal_links?.map(
                (link, i) => (i === index ? { ...link, [field]: value } : link)
            );

        updateProposalLinks(newLinks);
        if (field === 'prop_link') {
            if (value === '') {
                setLinksErrors((prev) => {
                    const { [index]: removed, ...rest } = prev;
                    return rest;
                });
            } else if (typeof value === 'string' && value.length > 2048) {
                setLinksErrors((prev) => ({
                    ...prev,
                    [index]: {
                        ...prev[index],
                        url: 'URL must be 2048 characters or less',
                    },
                }));
            } else if (typeof value === 'string' && value.length > 0) {
                const isValid = isValidURLFormat(value);
                setLinksErrors((prev) => ({
                    ...prev,
                    [index]: {
                        ...prev[index],
                        url: isValid ? '' : 'Invalid URL format',
                    },
                }));
            } else {
                setLinksErrors((prev) => {
                    const { [index]: removed, ...rest } = prev;
                    return rest;
                });
            }
        } else if (field === 'prop_link_text') {
            if (value.length > 255) {
                setLinksErrors((prev) => ({
                    ...prev,
                    [index]: {
                        ...prev[index],
                        text: 'Text must be 255 characters or less',
                    },
                }));
            } else {
                setLinksErrors((prev) => ({
                    ...prev,
                    [index]: {
                        ...prev[index],
                        text: '',
                    },
                }));
            }
        }
    };
    const handleAddLink = () => {
        const currentLinks =
            budgetDiscussionData.bd_further_information?.proposal_links || [];
        if (currentLinks.length < maxLinks) {
            updateProposalLinks([
                ...currentLinks,
                { prop_link: '', prop_link_text: '' },
            ]);
        }
    };

    const handleRemoveLink = (index) => {
        const newLinks =
            budgetDiscussionData.bd_further_information?.proposal_links?.filter(
                (_, i) => i !== index
            );

        updateProposalLinks(newLinks);

        // Uklanjanje grešaka za uklonjeni link
        setLinksErrors((prev) => {
            const { [index]: removed, ...rest } = prev;
            return rest;
        });
    };

    return (
        <Box sx={{ align: 'center' }}>
            <Typography
                variant='body1'
                fontWeight={400}
                mb={2}
                sx={{ textAlign: 'center', mt: 2 }}
            >
                (maximum of {maxLinks} entries)
            </Typography>
            {budgetDiscussionData.bd_further_information?.proposal_links?.map(
                (link, index) => (
                    <Box
                        key={index}
                        display='flex'
                        flexDirection='row'
                        mb={3}
                        position='relative'
                    >
                        <Box display='flex' flexDirection='column' flexGrow={1}>
                            <Box display={'flex'} justifyContent={'flex-end'}>
                                <IconButton
                                    onClick={() => handleRemoveLink(index)}
                                    data-testid='link-wrapper-remove-link-button'
                                >
                                    <DeleteOutlineIcon
                                        color='primary'
                                        sx={{ fontSize: 24 }}
                                    />
                                </IconButton>
                            </Box>
                            <Box>
                                <PdfInput
                                    label={`Link #${index + 1} URL`}
                                    value={link.prop_link || ''}
                                    onChange={(e) =>
                                        handleLinkChange(
                                            index,
                                            'prop_link',
                                            e.target.value
                                        )
                                    }
                                    placeholder='https://website.com'
                                    layoutStyles={{ mb: 2 }}
                                    dataTestId={`link-${index}-url-input`}
                                    errorMessage={
                                        linksErrors[index]?.url || undefined
                                    }
                                    errorDataTestId={`link-${index}-url-input-error`}
                                />
                                <PdfInput
                                    label={`Link #${index + 1} Text`}
                                    value={link.prop_link_text || ''}
                                    onChange={(e) =>
                                        handleLinkChange(
                                            index,
                                            'prop_link_text',
                                            e.target.value
                                        )
                                    }
                                    placeholder='Text'
                                    layoutStyles={{ mb: 2 }}
                                    dataTestId={`link-${index}-text-input`}
                                    errorMessage={
                                        linksErrors[index]?.text || undefined
                                    }
                                    errorDataTestId={`link-text-${index}-url-input-error`}
                                />
                            </Box>
                        </Box>
                    </Box>
                )
            )}
            {budgetDiscussionData.bd_further_information?.proposal_links
                ?.length < maxLinks && (
                <Box
                    sx={{
                        display: 'flex',
                        justifyContent: 'center',
                        mt: 2,
                    }}
                >
                    <Button
                        variant='text'
                        size='extraLarge'
                        startIcon={
                            <IconPlus fill={theme.palette.primary.main} />
                        }
                        onClick={handleAddLink}
                        data-testid='add-link-button'
                    >
                        Add link
                    </Button>
                </Box>
            )}
        </Box>
    );
};

export default BudgetDiscussionLinkManager;
