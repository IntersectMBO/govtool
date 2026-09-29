import { createTheme } from '@mui/material/styles';
import { theme as govtoolTheme } from '@/theme';

// pdf-ui renders on GovTool's theme: its palette, breakpoints, typography,
// shadows and component overrides, including GovTool's MuiInputBase pill.
// Two overrides remain, scoped to the MUI components pdf-ui still uses:
//   - MuiCard: the version and poll panels are MUI Cards; this gives them
//     GovTool's card shadow.
//   - MuiTextField: the TextFields pdf-ui keeps on purpose (the selects and
//     the fields tests reach with getByLabel) get the GovTool pill look.
//     Plain inputs and text areas are GovTool Field.Input / Field.TextArea
//     and do not use TextField.

const { lightBlue, secondaryBlue, neutralWhite } = govtoolTheme.palette;

const compatComponents = {
    MuiCard: {
        styleOverrides: {
            root: {
                boxShadow: '0px 4px 15px 0px #DDE3F5',
                borderRadius: '16px',
            },
        },
        variants: [
            {
                props: { variant: 'outlined' },
                style: { boxShadow: 'none', border: `1px solid ${lightBlue}` },
            },
        ],
    },
    // GovTool's MuiInputBase override pads every InputBase root 8px 16px,
    // which doubles up with the outlined input's own padding. Scoped to
    // TextField, so Field.Input (a bare InputBase) keeps GovTool's look.
    MuiTextField: {
        styleOverrides: {
            root: {
                '& .MuiInputBase-root': {
                    padding: 0,
                    borderRadius: 50,
                    backgroundColor: neutralWhite,
                },
                '& .MuiInputBase-adornedStart': { paddingLeft: 16 },
                '& .MuiInputBase-adornedEnd': { paddingRight: 16 },
                '& .MuiInputBase-multiline': {
                    borderRadius: 24,
                    padding: '12px 16px',
                },
                '& .MuiInputBase-input:not(.MuiInputBase-inputMultiline)': {
                    padding: '12px 16px',
                },
                '& .MuiInputBase-input.MuiInputBase-inputAdornedStart': {
                    paddingLeft: 8,
                },
                // Room for the select's arrow icon.
                '& .MuiSelect-select.MuiInputBase-input.MuiOutlinedInput-input':
                    { paddingRight: 32 },
                '& .MuiOutlinedInput-notchedOutline': {
                    borderColor: secondaryBlue,
                },
                '& .MuiInputLabel-root:not(.MuiInputLabel-shrink)': {
                    transform: 'translate(16px, 12px) scale(1)',
                },
                '& .MuiInputLabel-shrink': {
                    transform: 'translate(20px, -9px) scale(0.75)',
                },
                '& .MuiOutlinedInput-notchedOutline legend': {
                    marginLeft: 6,
                },
            },
        },
    },
};

const pdfTheme = createTheme(govtoolTheme, { components: compatComponents });
// --------------------------- end of compat layer ---------------------------

export default pdfTheme;
