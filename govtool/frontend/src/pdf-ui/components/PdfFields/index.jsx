import { Box } from '@mui/material';
import { Typography } from '@atoms';
import { Field } from '@molecules';

// Thin pdf-ui wrappers over GovTool's Field.Input, Field.TextArea and
// Field.Checkbox. They only adapt pdf-ui's props (a `required` asterisk, an
// explicit maxLength, the pdf helper and character-count testids); the look is
// GovTool's.
//
// Testid placement:
//   - PdfInput: `dataTestId` is on the <input> (Field.Input's inputProps).
//   - PdfTextArea: `dataTestId` is on the <textarea>.
//   - PdfCheckbox: `dataTestId` is on the <input type=checkbox>.
//   - `errorDataTestId` is on the error text, which only renders while
//     `errorMessage` is set.
//
// The labels are Typography, not <label>, so getByLabel cannot find them.
// Fields that tests reach through getByLabel stay MUI TextFields.

const fieldLabel = (label, required) =>
    label && required ? `${label} *` : label;

const helperTextColor = '#9792B5';

export function PdfInput({
    label,
    required,
    dataTestId,
    maxLength,
    inputProps,
    errorMessage,
    errorDataTestId,
    helpfulText,
    helpfulTextDataTestId,
    ...rest
}) {
    return (
        <Field.Input
            label={fieldLabel(label, required)}
            required={required}
            dataTestId={dataTestId}
            // Field.Input's own inputProps only carry the testid, and any
            // inputProps passed here replace them, so the testid goes in again.
            inputProps={{
                'data-testid': dataTestId,
                ...(maxLength !== undefined && { maxLength }),
                ...inputProps,
            }}
            errorMessage={errorMessage || undefined}
            errorDataTestId={errorDataTestId}
            helpfulText={errorMessage ? undefined : helpfulText || undefined}
            helpfulTextDataTestId={helpfulTextDataTestId}
            {...rest}
        />
    );
}

// Field.TextArea draws its own `n/max` counter inside the box. When pdf-ui's
// counter (with its testid) is shown below instead, the built-in one is hidden.
const hideBuiltInCounterSx = {
    '& > .MuiBox-root > .MuiTypography-root': { display: 'none' },
};

export function PdfTextArea({
    label,
    required,
    dataTestId,
    maxLength,
    value,
    errorMessage,
    errorDataTestId,
    helperText,
    helperTextDataTestId,
    counterDataTestId,
    layoutStyles,
    ...rest
}) {
    const showCounter = Boolean(counterDataTestId);
    const showHelperRow = !errorMessage && (Boolean(helperText) || showCounter);
    return (
        <Box sx={{ width: '100%', ...layoutStyles }}>
            <Field.TextArea
                label={fieldLabel(label, required)}
                required={required}
                data-testid={dataTestId}
                maxLength={maxLength}
                value={value ?? ''}
                errorMessage={errorMessage || undefined}
                errorDataTestId={errorDataTestId}
                layoutStyles={showCounter ? hideBuiltInCounterSx : undefined}
                {...rest}
            />
            {showHelperRow && (
                <Box
                    sx={{
                        display: 'flex',
                        justifyContent: 'space-between',
                        alignItems: 'flex-start',
                        gap: 2,
                        mt: 0.5,
                    }}
                >
                    {helperText ? (
                        <Typography
                            variant='caption'
                            component='span'
                            color={helperTextColor}
                            data-testid={helperTextDataTestId}
                        >
                            {helperText}
                        </Typography>
                    ) : (
                        <span />
                    )}
                    {showCounter && (
                        <Typography
                            variant='caption'
                            component='span'
                            color={helperTextColor}
                            data-testid={counterDataTestId}
                            sx={{ flexShrink: 0 }}
                        >
                            {`${value?.length || 0}/${maxLength}`}
                        </Typography>
                    )}
                </Box>
            )}
        </Box>
    );
}

// `onChange` receives the new checked value as a boolean. Field.Checkbox fires
// it from the checkbox and again from its clickable row, both times with the
// same value, so the handler must set state rather than toggle it.
export function PdfCheckbox({
    checked,
    onChange,
    dataTestId,
    errorMessage,
    ...rest
}) {
    const isChecked = Boolean(checked);
    const handleChange = (next) =>
        onChange?.(
            typeof next === 'boolean' ? next : Boolean(next?.target?.checked)
        );
    return (
        <Field.Checkbox
            checked={isChecked}
            value={isChecked}
            onChange={handleChange}
            dataTestId={dataTestId}
            errorMessage={errorMessage || undefined}
            {...rest}
        />
    );
}
