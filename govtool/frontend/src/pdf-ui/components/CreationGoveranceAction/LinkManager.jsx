import { useTheme } from '@emotion/react';
import {
    IconPlus,
} from '@intersect.mbo/intersectmbo.org-icons-set';
import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { Box, IconButton } from '@mui/material';
import { Button } from '@atoms';
import { PdfInput } from '../PdfFields';

import { isValidURLFormat } from '../../lib/utils';

const LinkManager = ({
    proposalData,
    setProposalData,
    linksErrors,
    setLinksErrors,
}) => {
    const theme = useTheme();

    const handleLinkChange = (index, field, value) => {
        let newLinks = proposalData?.proposal_links?.map((link, i) => {
            if (i === index) {
                return { ...link, [field]: value };
            }
            return link;
        });

        setProposalData({
            ...proposalData,
            proposal_links: newLinks,
        });
        if (field === 'prop_link' && value === '') {
            return setLinksErrors((prev) => {
                const { [index]: removed, ...rest } = prev;
                return rest;
            });
        }
        if (field === 'prop_link') {
            let urlError = '';
            if (value.length > 2048) {
                urlError = 'URL must be 2048 characters or less';
            } else if (value && !isValidURLFormat(value)) {
                urlError = 'Invalid URL format';
            }
            setLinksErrors((prev) => ({
                ...prev,
                [index]: {
                    ...prev[index],
                    url: urlError,
                },
            }));
        }
        if (field === 'prop_link_text') {
            let textError = '';
            if (value.length > 255) {
                textError = 'Text must be 255 characters or less';
            }
            if (value.trim() === '') {
                textError = 'Text cannot be empty';
            }
            setLinksErrors((prev) => {
                if (textError === '') {
                    const currentErrors = prev[index] || {};
                    const { text, ...otherErrors } = currentErrors;

                    if (Object.keys(otherErrors).length === 0) {
                        const { [index]: removed, ...rest } = prev;
                        return rest;
                    } else {
                        return {
                            ...prev,
                            [index]: otherErrors,
                        };
                    }
                }
                return {
                    ...prev,
                    [index]: {
                        ...prev[index],
                        text: textError,
                    },
                };
            });
        }
    };

    const handleAddLink = () => {
        setProposalData({
            ...proposalData,
            proposal_links: [
                ...proposalData?.proposal_links,
                { prop_link: '' },
            ],
        });
    };

    const handleRemoveLink = (index) => {
        let newLinks = proposalData?.proposal_links?.filter(
            (_, i) => i !== index
        );
        setProposalData({
            ...proposalData,
            proposal_links: newLinks,
        });

        // Remove errors for removed link
        setLinksErrors((prev) => {
            const { [index]: removed, ...rest } = prev;
            return rest;
        });
    };

    // GovTool's CreateGovernanceActionForm links: stacked fields with the
    // delete icon inside the URL field and a text "Add link" button below.
    return (
        <Box sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}>
            {proposalData?.proposal_links?.map((link, index) => (
                <Box
                    key={index}
                    sx={{ display: 'flex', flexDirection: 'column', gap: 3 }}
                >
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
                        dataTestId={`link-${index}-url-input`}
                        //link length limited to 255 characters
                        // maxLength={255}
                        errorMessage={linksErrors[index]?.url}
                        errorDataTestId={`link-${index}-url-input-error`}
                        endAdornment={
                            <IconButton
                                onClick={() => handleRemoveLink(index)}
                                data-testid='link-wrapper-remove-link-button'
                                size='small'
                                sx={{ p: 0.5, mr: -0.5 }}
                            >
                                <DeleteOutlineIcon
                                    color='primary'
                                    sx={{ height: 24, width: 24 }}
                                />
                            </IconButton>
                        }
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
                        dataTestId={`link-${index}-text-input`}
                        errorMessage={linksErrors[index]?.text}
                        errorDataTestId={`link-text-${index}-url-input-error`}
                    />
                </Box>
            ))}
            <Box sx={{ display: 'flex', justifyContent: 'center' }}>
                <Button
                    variant='text'
                    size='extraLarge'
                    startIcon={<IconPlus fill={theme.palette.primary.main} />}
                    onClick={handleAddLink}
                    data-testid='add-link-button'
                >
                    Add link
                </Button>
            </Box>
        </Box>
    );
};

export default LinkManager;
