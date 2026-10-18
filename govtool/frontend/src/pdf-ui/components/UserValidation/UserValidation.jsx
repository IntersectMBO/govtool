import {
    Box,
    Card,
    CardContent,
    List,
    ListItem,
} from '@mui/material';
import { Typography } from '@atoms';
import React, { useMemo } from 'react';
import { Link } from '@mui/material';
import { useAppContext } from '../../context/context';
import { loginUserToApp } from '../../lib/helpers';

// GovTool's inline text link (e.g. RolesAndResponsibilities): an MUI Link
// that takes the surrounding body text's size and weight.
const inlineLinkSx = {
    cursor: 'pointer',
    font: 'inherit',
};

const UserValidation = ({
    type = 'budget',
    drepCheck = false,
    drepRequired = false,
}) => {
    const {
        walletAPI,
        setUser,
        setOpenUsernameModal,
        user,
        clearStates,
        addSuccessAlert,
        addErrorAlert,
        addChangesSavedAlert,
    } = useAppContext();

    // walletAPI is the live host wallet, and null until it is ready.
    // Session sync, disconnect and token refresh live in the context provider.
    const handleLogin = async (trigerSignData, useDRepKey = false) => {
        await loginUserToApp({
            wallet: walletAPI,
            setUser: setUser,
            setOpenUsernameModal: setOpenUsernameModal,
            trigerSignData: trigerSignData ? true : false,
            clearStates: clearStates,
            isDRep: useDRepKey,
            addErrorAlert,
            addSuccessAlert,
            addChangesSavedAlert,
        });
    };

    const checkFunctionCall = () => {
        if (!walletAPI?.address) {
            const button = document.querySelector(
                '[data-testId="connect-wallet-button"]'
            );
            button?.click();
        } else if (!user) {
            handleLogin(true);
        } else if (!user?.user?.govtool_username) {
            setOpenUsernameModal({
                open: true,
                callBackFn: () => {},
            });
        } else if (drepCheck) {
            handleLogin(true, true);
        }
    };

    const checkTitleText = (type) => {
        switch (type) {
            case 'budget':
                return 'To submit a comment, you need to';
            case 'comment':
                return 'To submit a reply, you need to';
            case 'proposal':
                return 'To submit a proposal, you need to';
            case 'governance':
                return 'If this is your Proposal, to submit it, you need to';
            case 'sentiment':
                return 'To show sentiment, you need to';
            case 'drep-poll':
                return 'If you are a Drep, you need to';
            default:
                return '';
        }
    };

    const showValidationMessage = useMemo(() => {
        if (!walletAPI) {
            return (
                <Link
                    onClick={checkFunctionCall}
                    data-testId='connect-wallet-link'
                    sx={inlineLinkSx}
                >
                    connect a Cardano wallet
                </Link>
            );
        }

        if (!user) {
            return (
                <Link
                    onClick={checkFunctionCall}
                    data-testId='verify-user-link'
                    sx={inlineLinkSx}
                >
                    verify yourself by signing a transaction
                </Link>
            );
        }

        if (!user?.user?.govtool_username) {
            return (
                <Link
                    onClick={checkFunctionCall}
                    data-testId='create-govtool-display-name-link'
                    sx={inlineLinkSx}
                >
                    create a GovTool Display Name
                </Link>
            );
        }
        if (drepRequired && drepCheck) {
            return (
                <Link
                    onClick={checkFunctionCall}
                    data-testId='verify-drep-link'
                    sx={inlineLinkSx}
                >
                    verify your status as a DRep.
                </Link>
            );
        }
    }, [user, walletAPI, drepCheck, drepRequired]);

    return (
        <Box
            sx={{
                display: 'flex',
                flexDirection: { xxs: 'column', md: 'row' },
                justifyContent: 'space-between',
                alignItems: { xxs: 'flex-start', md: 'center' },
            }}
        >
            <Box
                sx={{
                    display: 'flex',
                    flexDirection: 'row',
                    flexWrap: 'wrap',
                    gap: 0.5,
                    marginTop: 0.3,
                    alignItems: 'center',
                }}
            >
                <Typography variant='body1' fontWeight={400}>
                    {checkTitleText(type)}
                </Typography>
                <Typography variant='body1' fontWeight={400}>
                    {showValidationMessage}
                </Typography>
                {type === 'drep-poll' && !user && (
                    <Typography variant='body1' fontWeight={400}>
                        and Drep key to vote. This is a two step process for
                        security.
                    </Typography>
                )}
            </Box>
        </Box>
    );
};

export default UserValidation;
