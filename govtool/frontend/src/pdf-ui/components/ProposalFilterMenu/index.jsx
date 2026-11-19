'use client';

import { useState } from 'react';
import {
    Box,
    ButtonBase,
    Checkbox,
    FormControlLabel,
    Popover,
} from '@mui/material';
import { ChevronDown } from 'lucide-react';

import { ds, RADIUS, SHADOWS } from '@/consts/designTokens';

// A "Proposal type: Info Action" button opening a checkbox list. Each
// option keeps the old filter menu's `${testId}-radio` and
// `${testId}-radio-wrapper` ids, which the forum E2E specs click.
const ProposalFilterMenu = ({
    label,
    options,
    selected,
    onToggle,
    onClear,
    allLabel = 'All',
    clearLabel = 'Clear',
    dataTestId,
}) => {
    const [anchor, setAnchor] = useState(null);
    const chosen = options.filter((option) => selected.includes(option.key));
    // None chosen and every one chosen both mean no filter.
    const summary =
        chosen.length === 0 || chosen.length === options.length
            ? allLabel
            : chosen.length === 1
              ? chosen[0].label
              : `${chosen.length} selected`;
    const menuId = `${dataTestId}-menu`;

    return (
        <>
            <ButtonBase
                aria-controls={anchor ? menuId : undefined}
                aria-expanded={Boolean(anchor)}
                aria-haspopup='true'
                data-testid={dataTestId}
                onClick={(event) => setAnchor(event.currentTarget)}
                sx={{
                    alignItems: 'center',
                    backgroundColor: '#FFFFFF',
                    border: `1px solid ${ds.gray['200']}`,
                    borderRadius: `${RADIUS.md}px`,
                    color: ds.gray['600'],
                    display: 'inline-flex',
                    fontFamily: 'inherit',
                    fontSize: 14,
                    gap: 0.75,
                    height: 40,
                    maxWidth: '100%',
                    minWidth: 0,
                    px: 1.5,
                    whiteSpace: 'nowrap',
                    '&.Mui-focusVisible': {
                        outline: `2px solid ${ds.primary['600']}`,
                    },
                }}
            >
                {label}
                <Box
                    component='span'
                    sx={{
                        color: ds.gray['900'],
                        fontWeight: 500,
                        minWidth: 0,
                        overflow: 'hidden',
                        textOverflow: 'ellipsis',
                    }}
                >
                    {summary}
                </Box>
                <ChevronDown
                    aria-hidden
                    color={ds.gray['700']}
                    size={16}
                    style={{ flexShrink: 0 }}
                />
            </ButtonBase>
            <Popover
                id={menuId}
                anchorEl={anchor}
                anchorOrigin={{ horizontal: 'right', vertical: 'bottom' }}
                onClose={() => setAnchor(null)}
                open={Boolean(anchor)}
                slotProps={{
                    paper: {
                        sx: {
                            border: `1px solid ${ds.gray['200']}`,
                            borderRadius: `${RADIUS.md}px`,
                            boxShadow: SHADOWS.drop,
                            minWidth: 240,
                            mt: 0.5,
                            py: 0.75,
                        },
                    },
                }}
                transformOrigin={{ horizontal: 'right', vertical: 'top' }}
            >
                <Box role='group' aria-label={label}>
                    {options.map((option) => (
                        <Box
                            key={option.key}
                            data-testid={`${option.testId}-radio-wrapper`}
                            sx={{
                                px: 1.5,
                                '&:hover': { backgroundColor: ds.gray['50'] },
                            }}
                        >
                            <FormControlLabel
                                control={
                                    <Checkbox
                                        checked={selected.includes(option.key)}
                                        data-testid={`${option.testId}-radio`}
                                        onChange={() => onToggle(option.key)}
                                        size='small'
                                    />
                                }
                                label={option.label}
                                slotProps={{ typography: { fontSize: 14 } }}
                                sx={{ m: 0, width: '100%' }}
                            />
                        </Box>
                    ))}
                    {onClear && (
                        <ButtonBase
                            data-testid={`${dataTestId}-clear`}
                            onClick={() => {
                                onClear();
                                setAnchor(null);
                            }}
                            sx={{
                                color: ds.primary['700'],
                                fontFamily: 'inherit',
                                fontSize: 14,
                                fontWeight: 500,
                                justifyContent: 'flex-start',
                                mt: 0.5,
                                px: 1.5,
                                py: 0.75,
                                width: '100%',
                                '&.Mui-focusVisible': {
                                    outline: `2px solid ${ds.primary['600']}`,
                                },
                            }}
                        >
                            {clearLabel}
                        </ButtonBase>
                    )}
                </Box>
            </Popover>
        </>
    );
};

export default ProposalFilterMenu;
