import { Box, IconButton, Modal as MuiModal } from '@mui/material';
import { Button, ModalContents, ModalWrapper, Typography } from '@atoms';
import { ICONS, IMAGES } from '@consts';

// pdf-ui modals on GovTool's modal atoms (Modal + ModalWrapper + ModalHeader +
// ModalContents). The GovTool Modal atom takes no testid, so the MUI Modal is
// rendered here directly: `dataTestId` stays on the modal root, where the
// hand-built pdf-ui modals had it.
//
// ModalWrapper's own close control is an <img> that also calls GovTool's
// global closeModal(). It is always hidden here and replaced by an IconButton
// (role=button) in the same spot, which only calls `onClose`.

const closeButtonSx = {
    position: 'absolute',
    top: 16,
    right: 16,
    p: 1,
};

export function PdfModalCloseButton({ onClick, dataTestId, sx }) {
    return (
        <IconButton
            aria-label='close'
            data-testid={dataTestId}
            onClick={onClick}
            sx={{ ...closeButtonSx, ...sx }}
        >
            <img alt='' src={ICONS.closeIcon} width={24} height={24} />
        </IconButton>
    );
}

export function PdfModal({
    open,
    onClose,
    dataTestId,
    wrapperDataTestId,
    closeButtonDataTestId,
    hideCloseButton = false,
    variant = 'modal',
    sx,
    children,
    ...modalProps
}) {
    return (
        <MuiModal
            open={Boolean(open)}
            onClose={onClose}
            disableAutoFocus
            data-testid={dataTestId}
            {...modalProps}
        >
            <Box sx={{ outline: 'none' }}>
                <ModalWrapper
                    variant={variant}
                    hideCloseButton
                    dataTestId={
                        wrapperDataTestId ??
                        (dataTestId ? `${dataTestId}-wrapper` : 'modal')
                    }
                    sx={sx}
                >
                    {!hideCloseButton && onClose && (
                        <PdfModalCloseButton
                            onClick={onClose}
                            dataTestId={closeButtonDataTestId}
                        />
                    )}
                    {children}
                </ModalWrapper>
            </Box>
        </MuiModal>
    );
}

const STATUS_IMAGES = {
    warning: IMAGES.warningImage,
    success: IMAGES.successImage,
    info: ICONS.timerIcon,
};

// Long labels wrap on narrow screens instead of overflowing the modal.
const statusButtonSx = {
    margin: '0 auto',
    width: '100%',
    whiteSpace: 'normal',
    height: 'auto',
    minHeight: 48,
};

function StatusButton({ button, variant, sx }) {
    if (!button) return null;
    const { label, dataTestId, sx: buttonSx, ...rest } = button;
    return (
        <Button
            data-testid={dataTestId}
            variant={variant}
            size='extraLarge'
            sx={{
                ...statusButtonSx,
                ...sx,
                ...buttonSx,
            }}
            {...rest}
        >
            {label}
        </Button>
    );
}

// Mirrors GovTool's organisms/Modal/StatusModal: a status image, a centred
// 28/500 title, centred 16/400 body text, then a full-width contained button
// and optional outlined ones. Local state and callbacks stay with the caller;
// it does not go through GovTool's global openModal().
export function PdfStatusModal({
    open,
    onClose,
    dataTestId,
    wrapperDataTestId,
    closeButtonDataTestId,
    hideCloseButton,
    status,
    icon,
    title,
    titleId,
    titleDataTestId,
    titleComponent = 'h2',
    message,
    messageId,
    messageDataTestId,
    children,
    primaryButton,
    secondaryButton,
    tertiaryButton,
    sx,
    ...modalProps
}) {
    const image = STATUS_IMAGES[status];
    return (
        <PdfModal
            open={open}
            onClose={onClose}
            dataTestId={dataTestId}
            wrapperDataTestId={wrapperDataTestId}
            closeButtonDataTestId={closeButtonDataTestId}
            hideCloseButton={hideCloseButton}
            sx={sx}
            {...modalProps}
        >
            {icon ??
                (image && (
                    <img
                        alt='Status icon'
                        src={image}
                        style={{
                            height: '84px',
                            margin: '0 auto',
                            width: '84px',
                        }}
                    />
                ))}
            {title && (
                <Typography
                    id={titleId}
                    data-testid={titleDataTestId}
                    component={titleComponent}
                    variant='headline5'
                    sx={{
                        mt: icon || image ? '34px' : 0,
                        mb: '8px',
                        textAlign: 'center',
                        wordBreak: 'break-word',
                    }}
                >
                    {title}
                </Typography>
            )}
            <ModalContents>
                {message && (
                    <Typography
                        id={messageId}
                        data-testid={messageDataTestId}
                        component='p'
                        fontWeight={400}
                        sx={{
                            textAlign: 'center',
                            wordBreak: 'break-word',
                            whiteSpace: 'pre-line',
                        }}
                    >
                        {message}
                    </Typography>
                )}
                {children}
            </ModalContents>
            {(primaryButton || secondaryButton || tertiaryButton) && (
                <Box
                    sx={{
                        display: 'flex',
                        flexDirection: 'column',
                        gap: 3,
                        mt: '38px',
                    }}
                >
                    <StatusButton button={primaryButton} variant='contained' />
                    <StatusButton button={secondaryButton} variant='outlined' />
                    <StatusButton button={tertiaryButton} variant='outlined' />
                </Box>
            )}
        </PdfModal>
    );
}

export default PdfModal;
