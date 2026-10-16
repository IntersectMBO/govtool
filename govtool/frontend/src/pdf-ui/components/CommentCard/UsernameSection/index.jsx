import React from 'react';

import { Box, Chip, Link } from '@mui/material';
import { Typography } from '@atoms';
import ValidationCheckmark from '../../../assets/svg/ValidationCheckmark';
import { dRepLinkSx, dRepTagSx } from '../commentStyles';

// Name row in the DRep directory card style: the name in body1 600, the
// DRep tag as a StatusPill-like chip, the DRep name and id as blue links.
const UsernameSection = ({ drepData, comment }) => {
    return (
        <Box>
            <Box
                sx={{
                    display: 'flex',
                    alignItems: 'center',
                    gap: 1,
                }}
            >
                <Typography
                    variant='body1'
                    component='h6'
                    sx={{ wordBreak: 'break-word' }}
                >
                    @{comment?.attributes?.user_govtool_username || ''}
                </Typography>
                {comment?.attributes?.user_is_validated === true ? (
                    <ValidationCheckmark />
                ) : null}
                {drepData?.view && <Chip data-testid='dRep-tag' label='DRep' size='small' sx={dRepTagSx} />}
            </Box>

            <Box
                sx={{
                    display: 'flex',
                    alignItems: 'center',
                    flexWrap: 'wrap',
                    columnGap: 1,
                }}
            >
                {drepData?.givenName && (
                    <Link
                        href={'/connected/drep_directory/' + drepData?.view}
                        target='_blank'
                        rel='noopener noreferrer'
                        underline='none'
                        sx={dRepLinkSx}
                    >
                        <Typography
                            variant='caption'
                            component='span'
                            data-testid='dRep-given-name'
                        >
                            {drepData?.givenName || ''}
                        </Typography>
                    </Link>
                )}

                {drepData?.view && (
                    <Link
                        href={'/connected/drep_directory/' + drepData?.view}
                        target='_blank'
                        rel='noopener noreferrer'
                        underline='none'
                        sx={dRepLinkSx}
                    >
                        <Typography
                            variant='caption'
                            component='span'
                            data-testid='dRep-id'
                        >
                            {drepData?.view?.slice(0, 26) + '...' || ''}
                        </Typography>
                    </Link>
                )}
            </Box>
        </Box>
    );
};

export default UsernameSection;
