import { Box, IconButton } from '@mui/material';
import { Button, Typography } from '@atoms';
import { ICONS, IMAGES } from '@consts';
import { useState } from 'react';
import { useAppContext } from '../../context/context';
import { updateUser } from '../../lib/api';
import { useTheme } from '@emotion/react';
import { PdfModal } from '../PdfModal';
import { PdfInput } from '../PdfFields';

// Modal buttons can wrap on narrow screens (GovTool modal buttons are 48px).
const modalButtonSx = {
    whiteSpace: 'normal',
    height: 'auto',
    minHeight: 48,
};

// Heading row: the title and its close IconButton, as in GovTool's modals.
const headingRowSx = {
    display: 'flex',
    flexDirection: 'row',
    justifyContent: 'space-between',
    alignItems: 'flex-start',
    gap: 2,
};

// GovTool modal body copy: 16/400 in textBlack.
const bodyTextSx = {
    mt: 1,
    mb: 3,
};

const closeIcon = (
    <img alt='' src={ICONS.closeIcon} width={24} height={24} />
);

const UsernameModal = ({ open, handleClose: close, setPDFUsername }) => {
    const theme = useTheme();
    const { setUser, setOpenUsernameModal } = useAppContext();
    const [username, setUsername] = useState('');
    const [step, setStep] = useState(1);
    const [usernameError, setUsernameError] = useState('');

    const validateUsername = (username) => {
        if (username === '') {
            setUsernameError('');
            return;
        }

        const usernamePattern = /^(?=.*[a-z])[a-z0-9._]{1,30}$/;
        const invalidStartPattern = /^[._]/;

        if (
            !usernamePattern.test(username) ||
            invalidStartPattern.test(username)
        ) {
            setUsernameError(
                'Invalid username. Only lower case letters, numbers, underscores, and periods are allowed. Username must be between 1 and 30 characters, contain at least one letter and cannot start with a period or underscore.'
            );
        } else {
            setUsernameError('');
        }
    };

    const handleUsernameChange = (e) => {
        const value = e.target.value.trim();
        setUsername(value);
        validateUsername(value);
    };

    const handleClose = (setFnToNUll = true) => {
        close();
        setStep(1);
        setUsername('');
        setUsernameError('');
        setFnToNUll
            ? setOpenUsernameModal((prev) => ({
                  ...prev,
                  callBackFn: () => {},
              }))
            : open?.callBackFn();
    };

    const handleNext = async () => {
        if (step === 1) {
            setStep(2);
        } else if (step === 2) {
            try {
                const updatedUser = await updateUser({
                    govtoolUsername: username,
                });

                if (!updatedUser) return;

                setUser((currentUser) => ({
                    ...currentUser,
                    user: updatedUser,
                }));

                if (setPDFUsername) {
                    setPDFUsername(updatedUser?.govtool_username);
                }

                setStep(3);
            } catch (error) {
                setStep(4);
                setUsername('');
                console.error(error);
            }
        }
    };

    const handleBack = () => {
        if (step === 2) {
            setStep(1);
        } else if (step === 3) {
            setStep(2);
        }
    };

    const renderStep = () => {
        switch (step) {
            case 1:
                return (
                    <Box>
                        <Box sx={headingRowSx}>
                            <Typography variant='headline5' component='h3'>
                                Hey, setup your username
                            </Typography>
                            <IconButton
                                onClick={handleClose}
                                data-testid='close-user-modal'
                            >
                                {closeIcon}
                            </IconButton>
                        </Box>

                        <Typography
                            variant='body1'
                            fontWeight={400}
                            sx={bodyTextSx}
                            color={(theme) => theme.palette.textBlack}
                        >
                            By setting up a unique username, you can submit a
                            proposal, participate in discussions, connect with
                            other members and maintains a respectful
                            environment. In the provided text field, please type
                            your desired username.
                        </Typography>

                        <PdfInput
                            label='Username'
                            layoutStyles={{
                                mb: 2,
                            }}
                            value={username || ''}
                            onChange={(e) => handleUsernameChange(e)}
                            required
                            dataTestId='username-input'
                            errorMessage={usernameError || undefined}
                            errorDataTestId='username-error-text'
                        />
                        <Button
                            data-testid='proceed-button'
                            variant='contained'
                            size='extraLarge'
                            fullWidth
                            sx={modalButtonSx}
                            disabled={
                                !Boolean(usernameError) &&
                                username?.length > 0 &&
                                username?.length <= 30
                                    ? false
                                    : true
                            }
                            onClick={handleNext}
                        >
                            Proceed with this username
                        </Button>
                    </Box>
                );
            case 2:
                return (
                    <Box>
                        <Box sx={headingRowSx}>
                            <Typography variant='headline5' component='h3'>
                                Are you sure you want to use "{username}"?
                            </Typography>
                            <IconButton
                                onClick={handleClose}
                                data-testid='close-user-modal'
                            >
                                {closeIcon}
                            </IconButton>
                        </Box>
                        <Typography
                            variant='body1'
                            fontWeight={400}
                            sx={bodyTextSx}
                            color={(theme) => theme.palette.textBlack}
                        >
                            Username cannot be changed in the future. Please
                            confirm it’s correct.
                        </Typography>
                        <PdfInput
                            label='Username'
                            layoutStyles={{
                                mb: 2,
                            }}
                            value={username || ''}
                            disabled
                            dataTestId='username-input'
                        />
                        <Box
                            sx={{
                                display: 'flex',
                                flexDirection: 'column',
                                gap: 3,
                            }}
                        >
                            <Button
                                variant='contained'
                                size='extraLarge'
                                fullWidth
                                sx={modalButtonSx}
                                onClick={handleNext}
                                data-testid='proceed-button'
                            >
                                Proceed with this username
                            </Button>
                            <Button
                                data-testid='no-change-button'
                                variant='outlined'
                                size='extraLarge'
                                fullWidth
                                sx={modalButtonSx}
                                onClick={handleBack}
                            >
                                No, Change
                            </Button>
                        </Box>
                    </Box>
                );
            case 3:
                return (
                    <>
                        <Box>
                            <Box sx={headingRowSx}>
                                <Typography variant='headline5' component='h3'>
                                    Username submitted!
                                </Typography>
                                <IconButton
                                    onClick={() => handleClose(false)}
                                    data-testid='close-user-modal'
                                >
                                    {closeIcon}
                                </IconButton>
                            </Box>
                        </Box>
                        <Box mt={4}>
                            <Button
                                data-testid='close-button'
                                variant='contained'
                                size='extraLarge'
                                fullWidth
                                sx={modalButtonSx}
                                onClick={() => handleClose(false)}
                            >
                                Close
                            </Button>
                        </Box>
                    </>
                );
            case 4:
                return (
                    <>
                        <Box textAlign='center'>
                            <Box
                                display='flex'
                                flexDirection='row'
                                justifyContent='center'
                                alignItems={'center'}
                            >
                                <img
                                    alt='Status icon'
                                    src={IMAGES.warningImage}
                                    style={{ height: '84px', width: '84px' }}
                                />
                            </Box>
                            <Typography
                                id='username-unavailable-title'
                                data-testid='username-unavailable-title'
                                mt='34px'
                                color={(theme) => theme.palette.textBlack}
                                variant='headline5'
                                component={'h5'}
                            >
                                Username Unavailable
                            </Typography>
                            <Typography
                                id='username-unavailable-description'
                                data-testid='username-unavailable-description'
                                mt={1}
                                color={(theme) => theme.palette.textBlack}
                                variant='body1'
                                fontWeight={400}
                                component={'p'}
                            >
                                The username you entered is already taken.
                                Please choose a different one.
                            </Typography>
                        </Box>
                        <Box mt='38px'>
                            <Button
                                data-testid='enter-new-username-button'
                                variant='contained'
                                size='extraLarge'
                                fullWidth
                                sx={modalButtonSx}
                                onClick={() => setStep(1)}
                            >
                                Enter a new username
                            </Button>
                        </Box>
                    </>
                );
            default:
                return null;
        }
    };

    return (
        <PdfModal
            open={open?.open}
            onClose={
                step === 3 ? () => handleClose() : () => handleClose(false)
            }
            dataTestId='setup-username-modal'
            // Each step renders its own close IconButton in its heading row.
            hideCloseButton
        >
            {renderStep()}
        </PdfModal>
    );
};

export default UsernameModal;
